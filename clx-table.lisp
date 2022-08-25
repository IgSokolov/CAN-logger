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
   :background (alloc-color colormap (lookup-color colormap "green"))))

;; each cell is a subwindow of table-window,
;; write fn to create coords
;; create 2D array of windows

(defstruct row
  cells
  value)
  
(defstruct table  
  window
  display
  content
  cache
  font
  cell-height
  col-width-list)

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
			       :save-under :on ;; TBC
			       :colormap colormap
			       :background (alloc-color colormap (lookup-color colormap "white"))))
		 (cell-gcontext (create-gcontext
				 :drawable cell-window
				 :font (open-font display font)
				 :line-style :solid
				 :background (screen-white-pixel screen)
				 :foreground (alloc-color colormap (lookup-color colormap "black")))))
	    ;;(map-window cell-window)	    
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
	  for cell = (nth col-n cells) ;; check!
	  for window = (car cell)
	  for gcontext = (cdr cell)
	  for string-width = (text-width gcontext string)	
	  for col-width = (nth col-n (table-col-width-list table)) do
	    (let ((xc (round (/ (- col-width string-width) 2)))
		  (yc (round (/ (+ cell-height font-height) 2))))	    	    
	      (clear-area window)
	      (draw-glyphs window gcontext xc yc string)	      
	      (sleep 0.1) ;; wtf!
	      (display-force-output display)))
    row))

;; depr!
(defun new-label-p (table label)
  (let ((db (table-content table)))
    (not (gethash label db))))

(define-condition empty-cache (error)
  ((label :initarg label :reader label) ;; fields not used!
   (can-id :initarg can-id :reader can-id)))

(defun register-label (table label can-id)
  (let ((row (pop (table-cache table))))
    (unless row
      (error 'empty-cache :label label :can-id can-id))
    (setf (gethash label (table-content table))
	  (write-to-row table row (list 0 1) (list (write-to-string can-id) label)))))

(defun write-value (table label value) ;; todo: opt no-overwrite
  (let ((db (table-content table)))
    (let ((row (gethash label db)))
      (setf (row-value row) value)
      (setf (gethash label (table-content table))
	    (write-to-row table row (list 2) (list (write-to-string value)))))))

(defun add-titles (table)
  (let ((row (pop (table-cache table))))
    (setf (gethash "titles" (table-content table))
	  (write-to-row table row (list 0 1 2) (list "can-id" "label" "value")))))

(defun clean-table (table)
  (let ((db (table-content table)))
    (maphash #'(lambda (label row)
		 (declare (ignore label))
		 (loop for cell in (row-cells row)
		       for window = (car cell) do
			 (clear-area window)))
	       db)))

(defun make-table-window-pair (main-window screen display colormap)
  (let* ((window (make-table-window main-window screen colormap 800 50 500 800))
	 (table (create-table screen display window colormap (list 60 200 60) 50 3)))
    (add-titles table)
    (values window table)))


(defun redraw-table (table)  
  (let ((db (table-content table)))
    (maphash #'(lambda (label row) ;; label = "flow", row = {value, 3 cells = (window . gcontext) for "can-id" "lable" "value"}
		 (format t "label = ~a~%" label)
		 (mapc #'(lambda (cell) (clear-area (car cell))) (row-cells row))
		 (if (string= label "titles")
		     (write-to-row table row (list 0 1 2) (list "can-id" "label" "value"))
		     (write-to-row table row (list 0 1 2) (list (write-to-string #x112) label (write-to-string (row-value row)))))) db)))
  
(defun show-table (n wt-stack)
  (let ((wt (nth n wt-stack)))
    (let ((window (car wt))
	  (table (cdr wt)))            
      (setf (window-priority window) :above)      
      (redraw-table table)            
      (display-force-output (table-display table)))))
			      
(defstruct wt-pool-unit
  label
  window
  table)

(defun find-wt-unit (wt-pool label)
  (find label wt-pool :key #'wt-pool-unit-label))

(defun make-random-data ()
    (let ((tags (list "pressure" "flow" "temperature" "velocity"))
	  (output))
      (loop :repeat 10 do
	(push (cons (nth (random (list-length tags)) tags) (random 100.0)) output))
      output))

(defun test ()
  (multiple-value-bind (display screen colormap) (make-default-display-screen-colormap)
    (let ((main-window (create-window ;; must be inhereted
			:parent (screen-root screen)
			:x 0
			:y 0
			:width 1500
			:height 1500
			:border (screen-black-pixel screen)
			:border-width 2
			:bit-gravity :center
			:colormap colormap
			:background (alloc-color colormap (lookup-color colormap "white")))))
      (map-window main-window)      
      (unwind-protect
           (let ((wt-pool) ;; wt-pool is a list of wt-pool-units
		 (wt-stack))	
	     (dolist (data (make-random-data))
	       (let ((label (car data))
		     (value (cdr data)))
		 (let ((wt-unit (find-wt-unit wt-pool label))) ;; wt-unit -> struct: label window table
		   (if wt-unit
		       (write-value (wt-pool-unit-table wt-unit) (wt-pool-unit-label wt-unit) value)
		       (progn
			 (if wt-pool
			     (let ((free-wt-unit (car wt-pool)))
			       (setq wt-unit (make-wt-pool-unit :label label :window (wt-pool-unit-window free-wt-unit) :table (wt-pool-unit-table free-wt-unit))))
			     (multiple-value-bind (window table) (make-table-window-pair main-window screen display colormap)
			       (let ((new-wt-unit (make-wt-pool-unit :label label :window window :table table)))				   
				 (push (cons window table) wt-stack) ;; for a switch button
				 (setq wt-unit new-wt-unit))))
			 (handler-case 
			     (register-label (wt-pool-unit-table wt-unit) (wt-pool-unit-label wt-unit) #x101) ;; todo: can-id look up
			   (empty-cache (c)
			     (declare (ignore c))
			     (multiple-value-bind (window table) (make-table-window-pair main-window screen display colormap)
			       (let ((new-wt-unit (make-wt-pool-unit :label label :window window :table table)))				   
				 (push (cons window table) wt-stack) ;; for a switch button
				 (setq wt-unit new-wt-unit)
				 (register-label (wt-pool-unit-table wt-unit) (wt-pool-unit-label wt-unit) #x101))))) ;; todo: can-id look up))))
			 (write-value (wt-pool-unit-table wt-unit) (wt-pool-unit-label wt-unit) value)
			 (push wt-unit wt-pool))))))
	     
	     (sleep 1)
	     (print "1")
	     (show-table 1 wt-stack)
	     (sleep 1)
	     (print "0")
	     (show-table 0 wt-stack)	     
	     ;; (sleep 1)
		 
	;;(map-window window2)
	;;(map-subwindows window2)
	     (display-finish-output display)
	     (sleep 1)
	     (close-display display))))))


