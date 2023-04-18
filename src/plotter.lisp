(in-package :can-logger.plotter)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency))

(defun linspace (start end n)
  (let ((dx (round (/ (- end start) n))))
    (values (loop for x from start
		  for i from 0 below n
		  collect (+ start (* dx i)))
	    dx)))

(defun linspace-2 (start end dx)
  (let ((n (round (/ (- end start) dx))))
    (loop for x from start
	  for i from 0 below n
	  collect (+ start (* dx i)))))

(defun map-y0-to-plot (y0 y-min y-max plot-window-size)
  (round (* plot-window-size (/ (- y-max y0) (- y-max y-min)))))

(defun reset-data (t-max dt data)
  "Assign a timestamp to each plotted point in data."
  (let ((i -1))
    (mapcar #'(lambda(point) (if point
				 (cons (- t-max (* (incf i) dt)) (cdr point))
				 (progn (incf i) NIL)))
	    data)))

(defstruct canvas-obj
  root
  canvas
  label
  data
  (data-counter 0))

(defstruct plot-env
  canvas-obj-db  
  (grid-c 0)  
  (y-min 0)
  (y-max 1))

(defun recompute-y-limits (y0 env)
  "Recompute ymin and y-max if y0 doesn't fit in beetween"
  (let ((redraw-p))
    (when (< y0 (plot-env-y-min env))
      (setq redraw-p t)
      (setf (plot-env-y-min env) (- y0 (* (abs y0) 0.1))))
    (when (> y0 (plot-env-y-max env))
      (setq redraw-p t)
      (setf (plot-env-y-max env) (+ y0 (* (abs y0) 0.1))))
    redraw-p))
 
(defun split-data (data)
  "Split data into plottable pieces. It is necessary
   because data can contain NILs"
  (flet ((delimiterp (item) (null item)))
    (split-sequence-by-delimiter data #'delimiterp)))

(defun create-horizontal-grid-lines (win gcontext plot-window-size x-start y-coords)
  (mapc (lambda (y) (draw-line win gcontext x-start y plot-window-size y)) y-coords))

(defun create-vertical-grid-lines (win gcontext plot-window-size x-coords)
  (mapc (lambda (x) (draw-line win gcontext x 0 x plot-window-size)) x-coords))

(defun get-plot-window-size (size start-offest-in-% end-offset-in-% y-start)
  "Set plot-window size. Offsets are in % of window-size."
  (let* ((x-start (round (* start-offest-in-% size)))
	 (x-end (round (* end-offset-in-% size)))
	 (plot-window-size (- x-end x-start)))
    (values size x-start y-start x-end plot-window-size)))

(defun make-x11-layers (root-window screen window-size colormap x-start y-start plot-window-size)
  "Create main windows and gcontexts"
  (let* ((main-window (create-window
		       :parent root-window
		       :x x-start
		       :y y-start
		       :width window-size
		       :height window-size
		       :border (screen-black-pixel screen)
		       :border-width 2
		       :bit-gravity :center
		       :colormap colormap
		       :background (alloc-color colormap (lookup-color colormap "white"))))
	 (plot-window (create-window
		       :parent main-window
		       :x 10
		       :y 50
		       :width plot-window-size
		       :height plot-window-size
		       :border (screen-black-pixel screen)
		       :border-width 2
		       :colormap colormap
		       :background (alloc-color colormap (lookup-color colormap "black"))))	 
         (grid (create-gcontext
                :drawable plot-window
		:line-style :solid
                :background (screen-white-pixel screen)
                :foreground (alloc-color colormap (lookup-color colormap "orange")))))    
    (map-window main-window)
    (map-window plot-window)
    (values main-window plot-window grid)))

(defun make-canvas (plot-window display screen colormap)
  (list
   (create-gcontext ;; main color for plot
   :drawable plot-window
   :fill-style :solid
   :font (open-font display "-bitstream-bitstream charter-bold-r-normal--0-0-0-0-p-0-iso8859-1")
   :background (screen-white-pixel screen)
   :foreground (alloc-color colormap (pick-up-color)))
   (create-gcontext ;; background for label area
   :drawable plot-window
   :fill-style :solid
   :background (screen-white-pixel screen)
   :foreground (alloc-color colormap (lookup-color colormap "white")))))

(defstruct plot-text-settings
  display
  labels-window
  labels-background
  labels-foreground
  background
  foreground
  font-height
  yc-ymax
  yc-ymin
  xc-ymin-ymax
  xc-title
  yc-title)

(defun make-plot-text-area (window plot-window-size screen
			    display colormap
			    &optional (font "-bitstream-bitstream charter-bold-r-normal--0-0-0-0-p-0-iso8859-1"))
  (let* ((x-start (round (* (drawable-width window) 0.1)))
	 (y-start 50)
	 (labels-window (create-window
		  :parent window
		  :x (+ 20 plot-window-size)
		  :y y-start
		  :width (- (drawable-width window) 20 plot-window-size 10) ;; 10 pixels margin
		  :height plot-window-size
		  :border (screen-black-pixel screen)
		  :border-width 2
		  :colormap colormap
		  :background (alloc-color colormap (lookup-color colormap "white")))))
    (map-window labels-window)
    (make-plot-text-settings
     :display display
     :labels-window labels-window
     :labels-background (create-gcontext
		  :drawable labels-window
		  :line-style :solid
		  :background (screen-white-pixel screen)
		  :foreground (alloc-color colormap (lookup-color colormap "white")))
     :labels-foreground (create-gcontext
		  :drawable labels-window
		  :line-style :solid
		  :background (screen-white-pixel screen)
		  :foreground (alloc-color colormap (lookup-color colormap "white")))
     :background (create-gcontext
		  :drawable window
		  :line-style :solid
		  :background (screen-white-pixel screen)
		  :foreground (alloc-color colormap (lookup-color colormap "white")))
     :foreground (create-gcontext
		  :drawable window
		  :font (open-font display font)
		  :line-style :solid
		  :background (screen-white-pixel screen)
		  :foreground (alloc-color colormap (lookup-color colormap "black")))
     :font-height (font-ascent (open-font display font))
     :yc-ymax (- y-start (font-ascent (open-font display font)))
     :yc-ymin (+ y-start plot-window-size (font-ascent (open-font display font)) 10)
     :xc-ymin-ymax (- plot-window-size 20)
     :xc-title x-start
     :yc-title (- y-start (font-ascent (open-font display font))))))

(defun draw-plot-text-y-min-y-max (env main-window text-layer text-layer-background font-ascent x-coord y-min-coord y-max-coord)
  (let ((y-min-string (format NIL "~10,3F" (plot-env-y-min env)))
	(y-max-string (format NIL "~10,3F" (plot-env-y-max env))))
    (draw-rectangle main-window text-layer-background x-coord (- y-max-coord font-ascent)
		    (text-width text-layer y-max-string) font-ascent :fill-p)
    (draw-rectangle main-window text-layer-background x-coord (- y-min-coord font-ascent)
		    (text-width text-layer y-min-string) font-ascent :fill-p)
    (draw-glyphs main-window text-layer x-coord y-min-coord y-min-string)
    (draw-glyphs main-window text-layer x-coord y-max-coord y-max-string)))

(defun draw-plot-text-plot-title (main-window text-layer x-coord y-coord dt t-max)
  (draw-glyphs main-window text-layer x-coord y-coord
	       (format NIL "dt = ~3,1F sec, t-max = ~3,1F sec" dt t-max)))
  
(defun add-canvas-obj (env plot-text-area label screen colormap plot-window)
  "Check in a new canvas in database"
  (let ((obj (make-canvas-obj
	      :root plot-window
	      :canvas (make-canvas plot-window (plot-text-settings-display plot-text-area)
				   screen
				   colormap)
	      :label label)))
    (push obj (plot-env-canvas-obj-db env))
    obj))

(defun fetch-canvas-obj (label db)
  "Retrieve a canvas from database"
  (let ((canvas-obj (find label db :test #'string= :key #'canvas-obj-label)))
    (values
     canvas-obj ;; canvas found
      (remove canvas-obj db)))) ;; the rest

(defun draw-initial-grid (display plot-window grid plot-window-size x-coords y-coords)
  "Create rectangular grid at start-up"
  (loop :repeat 2 do ;; 2 because otherwise no result obtained from x11 server. a bug?
    (create-horizontal-grid-lines plot-window grid plot-window-size 0 y-coords)
    (create-vertical-grid-lines plot-window grid plot-window-size
				(append x-coords (list (- plot-window-size 1))))
    (display-force-output display)
    (sleep 0.1))) ;; a hack. without sleeping no output. fix me.

(defun init-coordinate-settings (plot-window-size n-yticks t-max dt)
   (let ((y-coords (cdr (linspace 0 plot-window-size n-yticks)))			 
	 (data-length-max (round (/ t-max dt)))
	 (x-shift (round (* plot-window-size (/ dt t-max)))))
     (values y-coords x-shift data-length-max)))

(defun trim-data (obj data-length-max)
  "Keep fixed number of elements on a canvas"
  (if (= (canvas-obj-data-counter obj) data-length-max)
      (setf (canvas-obj-data obj) (butlast (canvas-obj-data obj)))
      (incf (canvas-obj-data-counter obj))))

(defun draw-rect-with-text (window background text-layer text x-coord y-coord font-ascent)
  (draw-rectangle window background x-coord (- y-coord font-ascent)
		  (text-width text-layer text) font-ascent :fill-p)
  (draw-glyphs window text-layer x-coord y-coord text))

(defun redraw-plot-window (env plot-window point-size plot-window-size grid y-coords dx-grid
			   x-end t-max dt)
  (clear-area plot-window)  
  ;; redraw grid				     
  (create-horizontal-grid-lines plot-window grid plot-window-size 0 y-coords)
  (create-vertical-grid-lines plot-window grid plot-window-size
			      (linspace-2 (- dx-grid (plot-env-grid-c env)) x-end dx-grid))	    
  ;; redraw data
  (dolist (canvas-obj (plot-env-canvas-obj-db env))
    (destructuring-bind (canvas-foreground canvas-background) (canvas-obj-canvas canvas-obj)
      (declare (ignore canvas-background))
      (let ((data (reset-data t-max dt (canvas-obj-data canvas-obj))))	
	(when data
	  (let ((data-list (remove-if #'null data)))
	    (let ((plot-buf-1) ;; for draw-lines
		  (plot-buf-2) ;; for draw-arcs
		  (y0-mapped-list (mapcar #'(lambda (item) (map-y0-to-plot (cdr item)
									   (plot-env-y-min env)
									   (plot-env-y-max env)
									   plot-window-size))
					  data-list))
		  (t0-mapped-list (mapcar #'(lambda (item)
					      (round (* plot-window-size (/ (car item) t-max))))
					  data-list)))	      
	      (loop for y0 in y0-mapped-list
		    for t0 in t0-mapped-list do
		      (push y0 plot-buf-1)
		      (push t0 plot-buf-1)
		      (loop for x in (list (* 2 pi) 0 point-size point-size
					   (- y0 (/ point-size 2)) (- t0 (/ point-size 2))) do
			(push x plot-buf-2)))	      
	      (if (> (list-length plot-buf-1) 2)
		  (progn
		    (draw-lines plot-window canvas-foreground plot-buf-1)
		    (draw-arcs plot-window canvas-foreground plot-buf-2 :fill-p))						   
		  (draw-arc plot-window canvas-foreground (- (first plot-buf-1) (/ point-size 2))
			    (- (second plot-buf-1) (/ point-size 2))
			    point-size point-size 0 (* 2 pi) :fill-p)))))))))

(defun draw-label (env pd canvas-obj plot-text-area canvas-foreground
		   canvas-background y0-mapped plot-window-size)
  (declare (ignore pd))
  (let ((butlast-point (cdr (find-if #'consp (cdr (canvas-obj-data canvas-obj))))))
    (when butlast-point
      (draw-rectangle (plot-text-settings-labels-window plot-text-area)
		      canvas-background
		      0 (- (map-y0-to-plot butlast-point (plot-env-y-min env)
					   (plot-env-y-max env) plot-window-size)
			   (plot-text-settings-font-height plot-text-area))
		      ;; clean 5 pixels more to rm artifacts
		      (+ (drawable-width (canvas-obj-root canvas-obj)) 5)
		      (+ (plot-text-settings-font-height plot-text-area) 5) :fill-p))
    
    (draw-rectangle (plot-text-settings-labels-window plot-text-area)
		    canvas-background
		    0 (- y0-mapped (plot-text-settings-font-height plot-text-area))
		    (+ (drawable-width (canvas-obj-root canvas-obj)) 5)
		    (+ (plot-text-settings-font-height plot-text-area) 5) :fill-p)

    (draw-glyphs (plot-text-settings-labels-window plot-text-area) canvas-foreground
		 0 y0-mapped (canvas-obj-label canvas-obj))))

(defun find-previous-point (lst)
  (loop for i in lst
	for n from 0
	when i
	  return (values n (cdr i))))
  
(defun plot-pd (pd env plot-text-area screen grid plot-window t-max dt x-end dx-grid
		point-size x-shift data-length-max plot-window-size y-coords colormap)
  "Plot plot-data (pd). _env_ is modified."
  (let ((y0 (plot-data-value pd))
	(label (plot-data-label pd)))
    (multiple-value-bind (canvas-obj rest-canvas-objs)	
	(fetch-canvas-obj label (plot-env-canvas-obj-db env))
      (declare (ignore rest-canvas-objs))
      (unless canvas-obj
	(setq canvas-obj (add-canvas-obj env plot-text-area label screen
					 colormap plot-window)))
      (destructuring-bind (canvas-foreground canvas-background) (canvas-obj-canvas canvas-obj)
	;; we are consing NIL because we dont need absolute timestamp 99.9 % of time.
	;; Only if we need to rescale and redraw the plot, NILs are filled with time values,
	;; which are computed at every redraw cycle.
	(push (cons NIL y0) (canvas-obj-data canvas-obj))      
	(trim-data canvas-obj data-length-max) ;; keep fixed number of elements to plot	
	(if (recompute-y-limits y0 env) ;; check if we need rescaling (bug here? see commit 0c04f5b)
	    (progn
	       ;; delete labels when redraw
	      (clear-area (plot-text-settings-labels-window plot-text-area))
	      (redraw-plot-window env plot-window point-size plot-window-size grid y-coords
				  dx-grid x-end t-max dt))	    
	    (let ((y0-mapped (map-y0-to-plot y0 (plot-env-y-min env)
					     (plot-env-y-max env)
					     plot-window-size)))
	      (multiple-value-bind (n-empty-points prev-point)
		  (find-previous-point (cdr (canvas-obj-data canvas-obj)))
		(draw-label env pd canvas-obj plot-text-area canvas-foreground canvas-background
			    y0-mapped plot-window-size)
		(draw-arc plot-window canvas-foreground (- (- plot-window-size (/ point-size 2)) 1)
			  (- y0-mapped (/ point-size 1)) point-size point-size 0 (* 2 pi) :fill-p)		
 		(when prev-point ;; the dataset must be larger then ((NIL . 0.0d0))	      
		  (let ((prev-point-mapped (map-y0-to-plot prev-point (plot-env-y-min env)
							   (plot-env-y-max env) plot-window-size)))
		    (draw-line plot-window canvas-foreground
			       (- plot-window-size (* x-shift n-empty-points))
			       prev-point-mapped (- plot-window-size 1) y0-mapped))))))))))

(defun draw-vertical-grid (env grid dx-grid x-shift plot-window plot-window-size)
  (if (>= (plot-env-grid-c env) dx-grid)
      (progn
	;; one pixel back to make the line visible
	(create-vertical-grid-lines plot-window grid plot-window-size (list (- plot-window-size 1)))
	(setf (plot-env-grid-c env) 0))
      (incf (plot-env-grid-c env) x-shift)))

(defun push-NIL-data (env data-length-max)
  (mapc #'(lambda (obj) (push NIL (canvas-obj-data obj))) (plot-env-canvas-obj-db env))
  (mapc #'(lambda (obj) (trim-data obj data-length-max)) (plot-env-canvas-obj-db env)))
  
(defparameter *stop* NIL)

(defun close-widget-plot ()
  (setq *stop* t))

(defun take-last-elements (queue)
  (let ((acc))
    (loop for data = (sb-concurrency:dequeue queue) do
      (if data
	  (push data acc)
	  (return-from take-last-elements
	    (nreverse (remove-duplicates acc :key #'plot-data-label :test #'string=)))))))

(defun make-widget-plot (&key main-window display screen colormap x-start y-start
			   size (grid-yticks 10) (grid-xticks 10) (t-max 10) data-queue dt)
  "Create plotting environment, fetch plot-data (pd) from a data queue and plot it."
  (setq *stop* NIL)
  (with-safe-exit-on-window-closed
    (let ((plot-window-size (round (* size 0.8)))
	  (x-end (+ x-start size)))
      (multiple-value-bind (main-window plot-window grid)
	  (make-x11-layers main-window screen (- x-end x-start) colormap
			   x-start y-start plot-window-size)
	(let ((plot-text-area (make-plot-text-area main-window
						   plot-window-size
						   screen display
						   colormap)))	  
	  (let ((env (make-plot-env)) ;; the _env_ lexical environment is modified. The rest ist const.
		(point-size 4)
		(t0 0)
		(NIL-canvas (car (make-canvas plot-window display screen colormap))))
	    (multiple-value-bind (x-coords dx-grid) (linspace 0 plot-window-size grid-xticks)
	      (multiple-value-bind (y-coords x-shift data-length-max)
		  (init-coordinate-settings plot-window-size grid-yticks t-max dt)		 
		(draw-initial-grid display plot-window grid plot-window-size x-coords y-coords)
		(draw-plot-text-plot-title main-window (plot-text-settings-foreground plot-text-area)
					   (plot-text-settings-xc-title plot-text-area)
					   (plot-text-settings-yc-title plot-text-area)
					   dt t-max)
		(loop until *stop* do
		  (setq t0 (get-internal-real-time))
		  (let ((pds (take-last-elements data-queue)))
		    (draw-plot-text-y-min-y-max env main-window
						(plot-text-settings-foreground plot-text-area)
						(plot-text-settings-background plot-text-area)
						(plot-text-settings-font-height plot-text-area)
						(plot-text-settings-xc-ymin-ymax plot-text-area)
						(plot-text-settings-yc-ymin plot-text-area)
						(plot-text-settings-yc-ymax plot-text-area))
		    (if pds
			(dolist (pd pds) ;; process data from queue
			  (plot-pd pd env plot-text-area screen grid plot-window t-max
				   dt x-end dx-grid point-size x-shift data-length-max
				   plot-window-size y-coords colormap))
			(push-NIL-data env data-length-max)))
		  (let ((x-shift-new (* (length (plot-env-canvas-obj-db env)) x-shift)))
		    (when (= x-shift-new 0)
		      (setq x-shift-new x-shift))		    
		    (copy-area plot-window NIL-canvas x-shift 0 plot-window-size
			       plot-window-size plot-window 0 0)
		    (draw-vertical-grid env grid dx-grid x-shift plot-window plot-window-size)		    
		    (create-horizontal-grid-lines plot-window grid plot-window-size
						  (- plot-window-size x-shift) y-coords)
		    (display-force-output display)		    
		    (let ((plot-time-overhead (/ (- (get-internal-real-time) t0) 1000000)))
		      (if (< dt plot-time-overhead dt)
			  (progn
			    (format t "Warning: cannot plot fast enough. dt = ~f s, plot-time-overhead = ~f s~%" dt plot-time-overhead)
			    (sleep dt))			  
			  (sleep (- dt plot-time-overhead))))))))))))))
