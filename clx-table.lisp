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
   :background (alloc-color colormap (lookup-color colormap "green"))))

;; each cell is a subwindow of table-window,
;; write fn to create coords
;; create 2D array of windows

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
	    (row))
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
			       :colormap colormap
			       :background (alloc-color colormap (lookup-color colormap "white"))))
		 (cell-gcontext (create-gcontext
				 :drawable cell-window
				 :font (open-font display font)
				 :line-style :solid
				 :background (screen-white-pixel screen)
				 :foreground (alloc-color colormap (lookup-color colormap "black")))))
	    ;;(map-window cell-window)
	    (push (cons cell-window cell-gcontext) row))
	  (incf x w))
	(push (nreverse row) cache))	
      (incf y cell-height))
    (map-window window) ;; comment later!
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
	(font-height (font-ascent (table-font table))))
    (loop for col-n in columns-list
	  for string in string-list
	  for cell = (nth col-n row)
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
	      
(defun new-label-p (table label)
  (let ((db (table-content table)))
    (not (gethash label db))))

(defun register-label (table label can-id) ; todo: cache NIL ?
  (let ((row (pop (table-cache table))))    
    (setf (gethash label (table-content table))
	  (write-to-row table row (list 0 1) (list (write-to-string can-id) label)))))

(defun write-value (table label value)
  (let ((db (table-content table)))
    (let ((row (gethash label db)))      
      (setf (gethash label (table-content table))
	    (write-to-row table row (list 2) (list (write-to-string value)))))))

(defun add-titles (table)
  (let ((row (pop (table-cache table))))
    (setf (gethash "titles" (table-content table))
	  (write-to-row table row (list 0 1 2) (list "can-id" "label" "value")))))

(defun clean-table (table)
  (let ((db (table-content table)))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (loop for cell in value
		       for window = (car cell) do
			 (clear-area window)))
	       db)))

(defun make-table-window-pair (main-window screen display colormap)
  (let* ((window (make-table-window main-window screen colormap 800 50 500 800))
	 (table (create-table screen display window colormap (list 60 200 60) 50 10)))
    (add-titles table)
    (cons window table)))

(defun show-window (n windows-stack)
  (let ((window (nth n windows-stack)))
    (map-window window)
    (map-subwindows window)))
		    	     
(defun find-wt (wt-pool label)
  (assoc label wt-pool :test 'string=))

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
           (let ((wt-pool)
		 (windows-stack))
	     ;;(list (make-table-window-pair main-window screen display colormap))))
	     (dolist (data (make-random-data))	       
	       (let ((label (car data))
		     (value (cdr data)))
		 (let ((wt (find-wt wt-pool label))) ;; wt = (window . table)		   
		   (if wt
		       (write-value (cddr wt) label value)
		       (if wt-pool
			   (progn
			     (register-label (cddar wt-pool) label #x101) ;; cdar wt-pool: insert new value to the last table
			     (write-value (cddar wt-pool) label value))
			   (let ((wt (make-table-window-pair main-window screen display colormap)))
			     (push (car wt) windows-stack)
			     (register-label (cdr wt) label #x101) ;; todo: can-id look up
			     (write-value (cdr wt) label value)
			     (push (cons label wt) wt-pool))))))	       	
	       ;; (show-window 0 windows-stack) ;; switch windows
	       (display-force-output display)
	       (sleep 1)))
		 
	;;(map-window window2)
	;;(map-subwindows window2)
	(display-finish-output display)
	(sleep 1)
	(close-display display)))))


