(in-package :can-logger.table)

(defun make-default-display-screen-colormap ()
  (let* ((display (open-default-display))
         (screen (display-default-screen display))
         (colormap (screen-default-colormap screen)))
    (values display screen colormap)))

(defun make-x11-layers (screen window-size colormap x-start plot-window-size)
  "Create main windows and gcontexts"
  (let* ((main-window (create-window
		       :parent (screen-root screen)
		       :x 0
		       :y 0
		       :width window-size
		       :height window-size
		       :border (screen-black-pixel screen)
		       :border-width 2
		       :bit-gravity :center
		       :colormap colormap
		       :background (alloc-color colormap (lookup-color colormap "white"))))
	 (plot-window (create-window
		       :parent main-window
		       :x x-start
		       :y 50
		       :width plot-window-size
		       :height plot-window-size
		       :border (screen-black-pixel screen)
		       :border-width 2
		       :colormap colormap
		       :background (alloc-color colormap (lookup-color colormap "green"))))	 
         (grid (create-gcontext
                :drawable plot-window
		:line-style :solid
                :background (screen-white-pixel screen)
                :foreground (alloc-color colormap (lookup-color colormap "orange")))))    
    (map-window main-window)
    (map-window plot-window)
    (values main-window plot-window grid)))

(defun make-plot-window (size start-offest-in-% end-offset-in-%) ;; offsets are in % of window-size
  "Set plot-window size"
  (let* ((x-start (round (* start-offest-in-% size)))
	 (x-end (round (* end-offset-in-% size)))
	 (plot-window-size (- x-end x-start)))
    (values size x-start x-end plot-window-size)))

(Defun test ()
  (multiple-value-bind (display screen colormap) (make-default-display-screen-colormap)
    (multiple-value-bind (window-size x-start x-end plot-window-size) (make-plot-window 1500 0.6 0.9)
      (multiple-value-bind (main-window plot-window grid) (make-x11-layers screen window-size colormap x-start plot-window-size)
	(unwind-protect
	     (display-force-output display)
	  (sleep 3)
	(display-finish-output display)
	(close-display display))))))
	
