(in-package :can-logger.button)

(defparameter *stop* NIL)

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
			 :event-mask '(:button-press)
			 :save-under :on
			 :background (alloc-color colormap (lookup-color colormap "green"))))
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
      (with-safe-exit-on-window-closed
	(loop named main until *stop* do
	  (format t "stop button..~%")
	  (event-case (display :force-output-p t :timeout 1)
	    (:button-press (window)
			   (print "button pressed.1")
			   (when (drawable-equal window button-window)
			     (print "button pressed.2")
			     (clear-area button-window)
			     (sleep 0.5)				
			     (draw-glyphs button-window button-gcontext xc yc label)
			     (sleep 0.1)
			     (print "send stop signal")
			     (sb-concurrency:enqueue t task-queue) ;; signal stop app.
			     (setq *stop* t)
			     t))
	    (otherwise ()
		       (print "otherwise")
		       t)))))
      (display-force-output display)))

