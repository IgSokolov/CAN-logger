(in-package :can-logger.tiles)

(defparameter *stop* NIL)

(defun close-widget-tiles ()
  (setq *stop* t))

(defstruct tile
  window
  gcontext
  gcontext-selected
  label
  font)

(defun draw-tile (tile)
  (let ((width (drawable-width (tile-window tile)))
	(height (drawable-height (tile-window tile)))
	(string-width (text-width (tile-gcontext tile) (format NIL "7FF")))
	(font-height (font-ascent (tile-font tile))))
    (let ((xc (round (/ (- width string-width) 2)))
	  (yc (round (/ (+ height font-height) 2))))	    	    
      (draw-glyphs (tile-window tile) (tile-gcontext tile) xc yc (format NIL "~x" (tile-label tile))))))
        
(defun highlight (tile)
  (draw-rectangle (tile-window tile) (tile-gcontext-selected tile) 0 0 (drawable-width (tile-window tile)) (drawable-height (tile-window tile)) :fill-p)
  (sleep 0.1)
  (draw-tile tile))

(defun create-tile (can-id window screen display colormap x0 y0 &optional (font "fixed"))
  (let* ((tile-window (create-window
		       :parent window
		       :x x0
		       :y y0
		       :width 30
		       :height 10
		       :border (screen-black-pixel screen)
		       :border-width 2
		       :bit-gravity :center
		       ;;:event-mask '(:button-press :button-release)
		       :colormap colormap
		       :background (alloc-color colormap (lookup-color colormap "white"))))
	 (tile-gcontext (create-gcontext
			 :drawable tile-window
			 :font (open-font display font)
			 :line-style :solid
			 :background (screen-white-pixel screen)
			 :foreground (alloc-color colormap (lookup-color colormap "black"))))
	 (tile-gcontext-selected (create-gcontext
			 :drawable tile-window
			 :font (open-font display font)
			 :line-style :solid
			 :background (screen-white-pixel screen)
			 :foreground (alloc-color colormap (lookup-color colormap "red")))))
    (map-window tile-window)
    (make-tile :window tile-window :gcontext tile-gcontext :gcontext-selected tile-gcontext-selected :label can-id :font (open-font display font))))  
  
(defun make-widget-tiles (&key main-window display screen colormap data-queue)
  (setq *stop* NIL)
  (let ((db)
	(window (create-window
		      :parent main-window
		      :x 5
		      :y 5
		      :width (- (drawable-width main-window) 15)
		      :height (- (drawable-height main-window) 15)
		      :border (screen-black-pixel screen)
		      :border-width 2
		      :bit-gravity :center
		      ;;:event-mask '(:button-press :button-release)
		      :colormap colormap
		      :background (alloc-color colormap (lookup-color colormap "white")))))
    (let ((tile-width 80)
	  (tile-height 40)
	  (max-x (- (drawable-width window) 5))
	  ;;(max-y (drawable-height window))
	  (xc 0)
	  (yc 0 ));; (list (cons can-id tile))
      (map-window main-window)
      (map-window window)
      (loop until *stop* for can-id = (sb-concurrency:dequeue data-queue) do
	(if can-id
	    (let ((tile (cdar (member can-id db :key #'car))))
	      (if tile
		  (highlight tile)
		  (let ((new-tile (create-tile can-id window screen display colormap xc yc)))		    
		    (if (> (- max-x xc) tile-width)
			(incf xc tile-width)
			(progn
			  (setq xc 0)
			  (incf yc (+ 5 tile-height))))
		    (push (cons can-id new-tile) db)
		    (draw-tile new-tile)))
	      (display-force-output display))
	    (sleep 0.01))))))
	    
			     

