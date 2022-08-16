(in-package :can-logger.table)

(defun make-default-display-screen-colormap ()
  (let* ((display (open-default-display))
         (screen (display-default-screen display))
         (colormap (screen-default-colormap screen)))
    (values display screen colormap)))

(defstruct table-layout
  table-window)

(defun create-table-layout (main-window screen colormap x-start y-start x-size y-size)
  (let ((tl (make-table-layout
	     :table-window (create-window
			    :parent main-window
			    :x x-start
			    :y y-start
			    :width x-size
			    :height y-size
			    :border (screen-black-pixel screen)
			    :border-width 2
			    :colormap colormap
			    :background (alloc-color colormap (lookup-color colormap "green"))))))
    (map-window main-window)
    (map-window (table-layout-table-window tl))))

;; (defun make-x11-layers (screen window-size colormap x-start plot-window-size)
;;   "Create main windows and gcontexts"
;;   (let* ((main-window (create-window ;; must be inhereted
;; 		       :parent (screen-root screen)
;; 		       :x 0
;; 		       :y 0
;; 		       :width window-size
;; 		       :height window-size
;; 		       :border (screen-black-pixel screen)
;; 		       :border-width 2
;; 		       :bit-gravity :center
;; 		       :colormap colormap
;; 		       :background (alloc-color colormap (lookup-color colormap "white"))))
;; 	 (plot-window (create-window
;; 		       :parent main-window
;; 		       :x x-start
;; 		       :y 50
;; 		       :width plot-window-size
;; 		       :height plot-window-size
;; 		       :border (screen-black-pixel screen)
;; 		       :border-width 2
;; 		       :colormap colormap
;; 		       :background (alloc-color colormap (lookup-color colormap "green"))))	 
;;          (grid (create-gcontext
;;                 :drawable plot-window
;; 		:line-style :solid
;;                 :background (screen-white-pixel screen)
;;                 :foreground (alloc-color colormap (lookup-color colormap "orange")))))    
;;     (map-window main-window)
;;     (map-window plot-window)
;;     (values main-window plot-window grid)))


(defun test ()
  (multiple-value-bind (display screen colormap) (make-default-display-screen-colormap)
    (let ((main-window (create-window
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
      (create-table-layout main-window screen colormap 800 50 200 500)
      (unwind-protect
	   (display-force-output display)
	(sleep 3)
	(display-finish-output display)
	(close-display display)))))
	
