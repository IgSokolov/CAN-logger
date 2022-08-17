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
   :border-width 2
   :colormap colormap
   :background (alloc-color colormap (lookup-color colormap "green"))))

;; each cell is a subwindow of table-window,
;; write fn to create coords
;; create 2D array of windows

(defstruct table  
  window
  content)

(defun create-table (screen window colormap col-width-list cell-height n-of-rows &optional (font "fixed"))
  (let ((y 0)
	(idx 0)
	(db (make-hash-table)))
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
			       :border-width 2
			       :bit-gravity :center
			       :colormap colormap
			       :background (alloc-color colormap (lookup-color colormap "white"))))
		 (cell-gcontext (create-gcontext
				 :drawable cell-window
				 :font font
				 :line-style :solid
				 :background (screen-white-pixel screen)
				 :foreground (alloc-color colormap (lookup-color colormap "black")))))
	    (map-window cell-window)
	    (push (cons cell-window cell-gcontext) row))
	  (incf x w))
	(setf (gethash idx db) row) 	
	(incf idx))
      (incf y cell-height))
    (make-table :window window
		:content db)))
	
(defun write-to-cell (table row-idx col-n string)
  (let ((window (table-window table))
	(row (gethash row-idx (table-content table))))
    (let ((cell (nth col-n row)))
      (clear-area (car cell))
      (draw-glyphs window (cdr cell) 0 0  string)
      (map-window window))))
  
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
      (unwind-protect
	   (let ((window (make-table-window main-window screen colormap 800 50 500 800)))
	     (map-window main-window)
	     (map-window window)
	     (display-force-output display)
	     (create-table screen window colormap (list 20 200 50) 50 3))
	     (sleep 5)
	     (display-finish-output display))
	(close-display display))))


