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
  font
  cell-height)

(defun create-table (screen display window colormap col-width-list cell-height n-of-rows &optional (font "fixed"))
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
	    (map-window cell-window)
	    (push (cons cell-window cell-gcontext) row))
	  (incf x w))
	(setf (gethash idx db) row) 	
	(incf idx))
      (incf y cell-height))
    (map-window window)
    (make-table :window window
		:display display
		:content db
		:font (open-font display font)
		:cell-height cell-height)))
	
(defun write-to-cell (table row-idx col-n string)
  (let* ((display (table-display table))
	 (row (gethash row-idx (table-content table)))
	 (cell (nth col-n row))
	 (font-height (font-ascent (table-font table)))
	 ;;(string-width (text-width (cdr cell) string))
	 (cell-height (table-cell-height table))
	 (yc (round (/ (+ cell-height font-height) 2))))
    (clear-area (car cell))
    (draw-glyphs (car cell) (cdr cell) 2 yc string)
    (sleep 0.1) ;; wtf!
    (display-force-output display)
    ))
  
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
           (let* ((window (make-table-window main-window screen colormap 800 50 500 800))
		  (table (create-table screen display window colormap (list 200 200 200) 50 10)))	     
             (write-to-cell table 1 1 "hello!")
	     (write-to-cell table 1 2 "hello!")
	     (sleep 3)
	     (display-finish-output display)
	     (close-display display))))))


