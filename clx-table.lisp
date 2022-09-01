(in-package :can-logger.table)

(defun make-default-display-screen-colormap ()
  (let* ((display (open-default-display))
         (screen (display-default-screen display))
         (colormap (screen-default-colormap screen)))
    (values display screen colormap)))

(defun make-table-window (main-window screen colormap x-start y-start x-size y-size)
  (create-window
   :parent main-window
   :x x-start
   :y y-start
   :width x-size
   :height y-size
   :border (screen-black-pixel screen)
   :border-width 1
   :colormap colormap
   :event-mask '(:structure-notify)
   :save-under :on
   :background (alloc-color colormap (lookup-color colormap "white"))))

(defstruct row
  cells
  value
  can-id)
  
(defstruct table  
  window
  display
  content
  cache
  font
  cell-height
  col-width-list
  (titles-list (list "can-id" "label" "value")))

(defun create-table (screen display window colormap col-width-list cell-height n-of-rows &optional (font "fixed"))
  (let ((y 0)
	(cache)
	(db (make-hash-table :test 'equal)))
    (dotimes (h n-of-rows)
      (let ((x 0)
	    (row (make-row)))
	(dolist (w col-width-list)
	  (let* ((cell-window (create-window
			       :parent window
			       :x x
			       :y y
			       :width w
			       :height cell-height
			       :border (screen-black-pixel screen)
			       :border-width 1
			       :bit-gravity :center
			       :save-under :on
			       :colormap colormap
			       :background (alloc-color colormap (lookup-color colormap "white"))))
		 (cell-gcontext (create-gcontext
				 :drawable cell-window
				 :font (open-font display font)
				 :line-style :solid
				 :background (screen-white-pixel screen)
				 :foreground (alloc-color colormap (lookup-color colormap "black")))))	 
	    (push (cons cell-window cell-gcontext) (row-cells row)))
	  (incf x w))
	(setf (row-cells row) (nreverse (row-cells row)))
	(push row cache))
      (incf y cell-height))
    (map-window window)
    (map-subwindows window)
    (display-force-output display)
    (make-table :window window
		:display display
		:content db
		:cache (nreverse cache)
		:font (open-font display font)
		:cell-height cell-height
		:col-width-list col-width-list)))

(defun write-to-row (table row columns-list string-list)
  (let ((display (table-display table))
	(cell-height (table-cell-height table))
	(font-height (font-ascent (table-font table)))
	(cells (row-cells row)))
    (loop for col-n in columns-list
	  for string in string-list
	  for cell = (nth col-n cells)
	  for window = (car cell)
	  for gcontext = (cdr cell)
	  for string-width = (text-width gcontext string)	
	  for col-width = (nth col-n (table-col-width-list table)) do
	    (let ((xc (round (/ (- col-width string-width) 2)))
		  (yc (round (/ (+ cell-height font-height) 2))))	    	    
	      (clear-area window)
	      (draw-glyphs window gcontext xc yc string)	      	      
	      (display-force-output display)))	      
    row))

(define-condition empty-cache (error)
  ())

(defun register-label (table label can-id)
  (let ((row (pop (table-cache table))))
    (unless row
      (error 'empty-cache))    
    (setf (row-can-id row) can-id)
    (setf (gethash label (table-content table))
	  (write-to-row table row (list 0 1) (list (write-to-string can-id) label)))))

(defun write-value (table label value)
  (let ((db (table-content table)))
    (let ((row (gethash label db)))
      (setf (row-value row) value)
      (setf (gethash label (table-content table))
	    (write-to-row table row (list 2) (list (write-to-string value)))))))

(defun add-titles (table)
  (let ((row (pop (table-cache table))))
    (setf (gethash "titles" (table-content table))
	  (write-to-row table row (list 0 1 2) (table-titles-list table)))))

(defun make-table-window-pair (main-window screen display colormap x y width height n-rows)
  (let* ((window (make-table-window main-window screen colormap x y width height))
	 (table (create-table screen display window colormap (list 60 200 60) 50 n-rows)))
    (add-titles table)
    (values window table)))

(defun redraw-table (table)  
  (let ((db (table-content table)))
    (maphash #'(lambda (label row)
		 ;; label = "flow"
		 ;; row = {value, can-id, 3 cells = (window . gcontext) for "can-id" "lable" "value"}
		 (mapc #'(lambda (cell) (clear-area (car cell))) (row-cells row))
		 (if (string= label "titles")
		     (write-to-row table row (list 0 1 2) (table-titles-list table))
		     (write-to-row table row (list 0 1 2)
				   (list (write-to-string (row-can-id row))
					 label
					 (write-to-string (row-value row))))))
	     db)))
  
(defstruct wt-pool-unit
  label
  can-id
  window
  table)

(defstruct window-table-stack
  stack
  (size 0)
  (pointer -1))
  
(defun add-wt-stack (window table wt-stack)
  (push (cons window table) (window-table-stack-stack wt-stack))
  (incf (window-table-stack-size wt-stack))
  (incf (window-table-stack-pointer wt-stack)))
  
(defun show-table (wt-stack direction)  
  (ccase direction
    (:next (when (<= (+ (window-table-stack-pointer wt-stack) 2) (window-table-stack-size wt-stack))
	     (incf (window-table-stack-pointer wt-stack))))
    (:prev (when (> (window-table-stack-pointer wt-stack) 0)
	     (decf (window-table-stack-pointer wt-stack)))))  
  (let ((wt (nth (window-table-stack-pointer wt-stack) (window-table-stack-stack wt-stack))))
    (let ((window (car wt))
	  (table (cdr wt)))      
      (setf (window-priority window) :above)
      (handler-case
	  (progn
	    (redraw-table table)            
	    (display-force-output (table-display table)))
	(XLIB:CLOSED-DISPLAY (c)
	  (declare (ignore c))
	  NIL)))))

(defun find-wt-unit (wt-pool label)
  (find label wt-pool :key #'wt-pool-unit-label))

(defparameter *stop* NIL)

(defun close-widget-table ()
  (setq *stop* t))

(defun make-paging-buttons (task-queue window display x y screen colormap size &optional (font "fixed"))
  (let ((left-win (create-window
		   :parent window
		   :x x
		   :y y
		   :width  size
		   :height size
		   :border (screen-black-pixel screen)
		   :border-width 2
		   :bit-gravity :center
		   :event-mask '(:button-press :button-release)
		   :colormap colormap
		   :background (alloc-color colormap (lookup-color colormap "green"))))
	  (right-win (create-window
		      :parent window
		      :x x
		      :y (+ y size 10)
		      :width size
		      :height size
		      :border (screen-black-pixel screen)
		      :border-width 2
		      :bit-gravity :center
		      :event-mask '(:button-press :button-release)
		      :colormap colormap
		      :background (alloc-color colormap (lookup-color colormap "green")))))
      (let ((left-g (create-gcontext
		     :drawable left-win
		     :font (open-font display font)
		     :line-style :solid
		     :background (screen-white-pixel screen)
		     :foreground (alloc-color colormap (lookup-color colormap "black"))))
	    (right-g (create-gcontext
		      :drawable right-win
		      :font (open-font display font)
		      :line-style :solid
		      :background (screen-white-pixel screen)
		      :foreground (alloc-color colormap (lookup-color colormap "black")))))
	(let ((next-image (read-bitmap-file "./images/next.xbm"))
	      (prev-image (read-bitmap-file "./images/prev.xbm"))
	      (next-pressed-image (read-bitmap-file "./images/next-pressed.xbm"))
	      (prev-pressed-image (read-bitmap-file "./images/prev-pressed.xbm")))
	  (map-window left-win)
	  (map-window right-win)
	  (sleep 0.1)
	  (put-image left-win left-g next-image :x 0 :y 0 :width size :height size :bitmap-p t)
          (put-image right-win right-g prev-image :x 0 :y 0 :width size :height size :bitmap-p t)	  
	  (sb-thread:make-thread
	   (lambda ()
	     (loop until *stop* do
				  (event-case (display :force-output-p t :timeout 0.1)
				    (:button-press (window)						   
						   (if (drawable-equal window left-win)
						       (progn
							 (put-image left-win left-g next-pressed-image :x 0 :y 0 :width size :height size :bitmap-p t)							 
							 (sb-concurrency:enqueue :next task-queue))
						       (progn
							 (put-image right-win right-g prev-pressed-image :x 0 :y 0 :width size :height size :bitmap-p t)
							 (sb-concurrency:enqueue :prev task-queue)))
						   t)
				    (:button-release (window)						     
						     (if (drawable-equal window left-win)
							 (put-image left-win left-g next-image :x 0 :y 0 :width size :height size :bitmap-p t)
							 (put-image right-win right-g prev-image :x 0 :y 0 :width size :height size :bitmap-p t))
						     t)
				    (otherwise () t)))))
	  (display-force-output display)))))

;; todo: add error signal
(defun make-widget-table (main-window display screen colormap data-queue x-table y-table width heigth n-rows x-buttons y-buttons)
  (setq *stop* NIL)
  (let ((wt-pool) ;; wt-pool is a list of wt-pool-units
	(wt-stack (make-window-table-stack))
	(paging-task-queue (sb-concurrency:make-queue :initial-contents NIL)))
    ;; start table switch daemon
    (sb-thread:make-thread
     (lambda ()
       (loop until *stop* do
	 (let ((task (sb-concurrency:dequeue paging-task-queue)))
	   (when task
	     (show-table wt-stack task)))
	 (sleep 0.1))))
    ;; read data
    (loop until *stop* for data = (sb-concurrency:dequeue data-queue) do
      (when data
	(let ((can-id (plot-data-can-id data))
	      (label (plot-data-label data))
	      (value (plot-data-value data)))
	  (let ((wt-unit (find-wt-unit wt-pool label))) ;; wt-unit -> struct: label window table
	    (if wt-unit
		(write-value (wt-pool-unit-table wt-unit) (wt-pool-unit-label wt-unit) value)
		(progn
		  (if wt-pool
		      (let ((free-wt-unit (car wt-pool)))
			(setq wt-unit (make-wt-pool-unit :can-id can-id :label label :window (wt-pool-unit-window free-wt-unit) :table (wt-pool-unit-table free-wt-unit))))
		      (multiple-value-bind (window table) (make-table-window-pair main-window screen display colormap x-table y-table width heigth n-rows)
			(make-paging-buttons paging-task-queue main-window display x-buttons y-buttons screen colormap 100)
			(let ((new-wt-unit (make-wt-pool-unit :can-id can-id :label label :window window :table table)))				   
			  (add-wt-stack window table wt-stack) ;; for a switch button
			  (setq wt-unit new-wt-unit))))
		  (handler-case 
		      (register-label (wt-pool-unit-table wt-unit) (wt-pool-unit-label wt-unit) (wt-pool-unit-can-id wt-unit))
		    (empty-cache (c)
		      (declare (ignore c))
		      (multiple-value-bind (window table) (make-table-window-pair main-window screen display colormap x-table y-table width heigth n-rows)
			(let ((new-wt-unit (make-wt-pool-unit :can-id can-id :label label :window window :table table)))				   
			  (add-wt-stack window table wt-stack)
			  (setq wt-unit new-wt-unit)
			  (register-label (wt-pool-unit-table wt-unit) (wt-pool-unit-label wt-unit) (wt-pool-unit-can-id wt-unit))))))
		  (write-value (wt-pool-unit-table wt-unit) (wt-pool-unit-label wt-unit) value)
		  (push wt-unit wt-pool)))))))))
;;(display-finish-output display)
;;(close-display display)
