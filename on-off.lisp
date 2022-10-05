(in-package :can-logger.on-off)

(defparameter *stop* NIL)

(defun close-widget-on-off ()
  (setq *stop* t))

(defstruct bit-field
  (bit 0)
  label
  value
  window
  gcontext
  gcontext-selected
  font)

(defun create-bit-field (label window screen display colormap x0 y0 &optional (font "fixed"))
  (let* ((bit-field-window (create-window
		       :parent window
		       :x x0
		       :y y0
		       :width (drawable-width window)
		       :height (font-ascent (open-font display font))
		       :border (screen-black-pixel screen)
		       :border-width 2
		       :bit-gravity :center
		       :colormap colormap
		       :background (alloc-color colormap (lookup-color colormap "gray"))))
	 (bit-field-gcontext (create-gcontext
			 :drawable bit-field-window
			 :font (open-font display font)
			 :line-style :solid
			 :background (screen-white-pixel screen)
			 :foreground (alloc-color colormap (lookup-color colormap "black"))))
	 (bit-field-gcontext-selected (create-gcontext
			 :drawable bit-field-window
			 :font (open-font display font)
			 :line-style :solid
			 :background (screen-white-pixel screen)
			 :foreground (alloc-color colormap (lookup-color colormap "green")))))
    (map-window bit-field-window)
    (make-bit-field :window bit-field-window :gcontext bit-field-gcontext :gcontext-selected bit-field-gcontext-selected :label label :font (open-font display font))))

(defun draw-bit-field (bit-field)
  (let ((width (drawable-width (bit-field-window bit-field)))
	(height (drawable-height (bit-field-window bit-field)))
	(string-width (text-width (bit-field-gcontext bit-field) (bit-field-label bit-field)))
	(font-height (font-ascent (bit-field-font bit-field))))
    (let ((xc (round (/ (- width string-width) 2)))
	  (yc (round (/ (+ height font-height) 2))))      
      (draw-glyphs (bit-field-window bit-field) (bit-field-gcontext bit-field) xc yc (bit-field-label bit-field)))))
        
(defun update (bit-field new-bit)
  (unless (= (bit-field-bit bit-field) new-bit)
    (setf (bit-field-bit bit-field) new-bit)    
    (if (= new-bit 0)
	(clear-area (bit-field-window bit-field))      
	(draw-rectangle (bit-field-window bit-field) (bit-field-gcontext-selected bit-field) 0 0 (drawable-width (bit-field-window bit-field)) (drawable-height (bit-field-window bit-field)) :fill-p))
    (sleep 0.1)
    (draw-bit-field bit-field)))

(defun make-widget-on-off (&key main-window display screen colormap data-queue)
  (setq *stop* NIL)
  (let ((db)
	(yc 0)
	(y-margin 5))
    (loop until *stop*
	  for pv = (sb-concurrency:dequeue data-queue) do
	    (if pv
		(let ((label (plot-data-label pv))
		      (bit (plot-data-value pv)))	    
		  (let ((bit-field (cdar (member label db :key #'car))))
		    (unless bit-field
		      (setq bit-field (create-bit-field label main-window screen display colormap 0 yc))
		      (incf yc (+ y-margin (font-ascent (bit-field-font bit-field))))
		      (push (cons label bit-field) db))
		    (update bit-field bit)			
		    (display-force-output display)))
		(sleep 0.01)))))
  
