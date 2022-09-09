(in-package :can-logger.button)


(defparameter *stop* NIL)

(defun close-widget-button ()
  (setq *stop* t))

(defun make-widget-button (&key main-window display screen colormap x y width height label task-queue (font "fixed"))
  (setq *stop* NIL)
  (let* ((button-window (create-window
			 :parent main-window
			 :x x
			 :y y
			 :width width
			 :height height
			 :border (screen-black-pixel screen)
			 :border-width 1
			 :colormap colormap
			 :event-mask '(:button-press :button-release)
			 :save-under :on
			 :background (alloc-color colormap (lookup-color colormap "white"))))
	 (button-gcontext (create-gcontext
			   :drawable button-window
			   :font (open-font display font)
			   :line-style :solid
			   :background (screen-white-pixel screen)
			   :foreground (alloc-color colormap (lookup-color colormap "black"))))
	 (font (open-font display font)) 
	 (font-height (font-ascent font))
	 (label-width (text-width button-gcontext label)))
    (map-window button-window)
    (sleep 0.1)
    (let ((xc (round (/ (- width label-width) 2)))
	  (yc (round (/ (+ height font-height) 2))))
      (draw-glyphs button-window button-gcontext xc yc label)
      (display-force-output display)
      (sb-thread:make-thread
       (lambda ()
	 (with-safe-exit-on-window-closed
	   (loop until *stop* do	   
	     (event-case (display :force-output-p t :timeout 0.5)
	       (:button-press (window)
			      (when (drawable-equal window button-window)
				(sb-concurrency:enqueue t task-queue)
				t))
	       (:button-release (window)
				t)
	       (otherwise () t))))))    
      (display-force-output display))))

