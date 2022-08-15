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
   because data can contain NILs when"
  (flet ((delimiterp (item) (null item)))
    (split-sequence-by-delimiter data #'delimiterp)))

(defun create-horizontal-grid-lines (win gcontext plot-window-size x-start y-coords)
  (mapc (lambda (y) (draw-line win gcontext x-start y plot-window-size y)) y-coords))

(defun create-vertical-grid-lines (win gcontext plot-window-size x-coords)
  ;;(assert (not (and x-start draw-last-line-only)))  
  (mapc (lambda (x) (draw-line win gcontext x 0 x plot-window-size)) x-coords))

(defun make-default-display-screen-colormap ()
  (let* ((display (open-default-display))
         (screen (display-default-screen display))
         (colormap (screen-default-colormap screen)))
    (values display screen colormap)))
   
(defun make-plot-window (size start-offest-in-% end-offset-in-%) ;; offsets are in % of window-size
  "Set plot-window size"
  (let* ((x-start (round (* start-offest-in-% size)))
	 (x-end (round (* end-offset-in-% size)))
	 (plot-window-size (- x-end x-start)))
    (values size x-start x-end plot-window-size)))

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
		       :y x-start
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

(defstruct canvas-obj
  canvas
  label
  data)

(defstruct plot-env
  canvas-obj-db
  (data-counter 0)
  (grid-c 0)  
  (y-min -1)
  (y-max 1))

(defun make-random-color ()
  (make-color :red (random 1.0) :blue (random 1.0) :green (random 1.0)))

(defun make-canvas (plot-window screen colormap)
  (create-gcontext
   :drawable plot-window
   :fill-style :solid
   :background (screen-white-pixel screen)
   :foreground (alloc-color colormap (make-random-color))))

(defun make-text-area (window screen display colormap x-end x-start &optional (font "fixed"))
  (values
   ;; background
   (create-gcontext
    :drawable window
    :line-style :solid
    :background (screen-white-pixel screen)
    :foreground (alloc-color colormap (lookup-color colormap "red")))
   ;; foreground
   (create-gcontext
    :drawable window
    :font (open-font display font)
    :line-style :solid
    :background (screen-white-pixel screen)
    :foreground (alloc-color colormap (lookup-color colormap "black")))
   (font-ascent (open-font display font))
   x-end                                              ;; Y for y-max
   (+ x-start (font-ascent (open-font display font))) ;; Y for y-min
   (+ x-end (font-ascent (open-font display font)))   ;; X for y min max
   x-start                                            ;; x for title
   (- x-start (font-ascent (open-font display font))))) ;; y for title
  

(defun add-canvas-obj (env label screen colormap plot-window)
  "Check in a new canvas in database"
  (let ((obj (make-canvas-obj
	      :canvas (make-canvas plot-window screen colormap)
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
    (create-vertical-grid-lines plot-window grid plot-window-size (append x-coords (list (- plot-window-size 1))))
    (display-force-output display)
    (sleep 0.1)))

(defun init-coordinate-settings (plot-window-size n-yticks t-max dt)
   (let ((y-coords (cdr (linspace 0 plot-window-size n-yticks)))			 
	 (data-length-max (round (/ t-max dt)))
	 (x-shift (round (* plot-window-size (/ dt t-max)))))
     (values y-coords x-shift data-length-max)))

(defun trim-data (env data-length-max)
  "Keep fixed number of elements on a canvas"
  (if (= (plot-env-data-counter env) data-length-max)
      (mapc #'(lambda (obj) (setf (canvas-obj-data obj) (butlast (canvas-obj-data obj)))) (plot-env-canvas-obj-db env))
      (incf (plot-env-data-counter env))))

(defun redraw-plot-window (env plot-window point-size plot-window-size grid y-coords dx-grid x-end t-max dt)
  (clear-area plot-window)  
  ;; redraw grid				     
  (create-horizontal-grid-lines plot-window grid plot-window-size 0 y-coords)
  (create-vertical-grid-lines plot-window grid plot-window-size (linspace-2 (- dx-grid (plot-env-grid-c env)) x-end dx-grid))	    
  ;; redraw data
  (dolist (canvas-obj (plot-env-canvas-obj-db env))
    (let* ((data (reset-data t-max dt (canvas-obj-data canvas-obj)))
	   (data-list (split-data data)))
      (dolist (data-item data-list)
	(let ((plot-buf-1) ;; for draw-lines
	      (plot-buf-2) ;; for draw-arcs
	      (y0-mapped-list (mapcar #'(lambda (item) (map-y0-to-plot (cdr item) (plot-env-y-min env) (plot-env-y-max env) plot-window-size)) data-item))
	      (t0-mapped-list (mapcar #'(lambda (item) (round (* plot-window-size (/ (car item) t-max)))) data-item)))
	  (loop for y0 in y0-mapped-list
		for t0 in t0-mapped-list do
		  (push y0 plot-buf-1)
		  (push t0 plot-buf-1)
		  (loop for x in (list (* 2 pi) 0 point-size point-size (- y0 (/ point-size 2)) (- t0 (/ point-size 2))) do
		    (push x plot-buf-2)))						     						   
	  (if (> (list-length plot-buf-1) 2)
	      (progn
		(draw-lines plot-window (canvas-obj-canvas canvas-obj) plot-buf-1)
		(draw-arcs plot-window (canvas-obj-canvas canvas-obj) plot-buf-2 :fill-p))						   
	      (draw-arc plot-window (canvas-obj-canvas canvas-obj) (- (first plot-buf-1) (/ point-size 2))
			(- (second plot-buf-1) (/ point-size 2)) point-size point-size 0 (* 2 pi) :fill-p)))))))

(defun plot-pd (pd env screen grid plot-window t-max dt x-end dx-grid point-size x-shift data-length-max plot-window-size y-coords colormap)
  "Plot plot-data (pd). _env_ is modified."
  (let ((y0 (plot-data-y pd))
	(label (plot-data-label pd)))			     
    (multiple-value-bind (canvas-obj rest-canvas-objs) (fetch-canvas-obj label (plot-env-canvas-obj-db env))			       
      ;; push NIL-data to the rest of canvasses
      (mapc #'(lambda (obj) (push NIL (canvas-obj-data obj))) rest-canvas-objs)			       
      (unless canvas-obj
	(setq canvas-obj (add-canvas-obj env label screen colormap plot-window)))
      (copy-area plot-window (canvas-obj-canvas canvas-obj) x-shift 0 plot-window-size plot-window-size plot-window 0 0)
      ;; we consing NIL because we dont need absolute timestamp 99.9 % of time.
      ;; Only if we need to rescale and redraw the plot, NILs are filled with time values,
      ;; which are computed at every redraw cycle.
      (push (cons NIL y0) (canvas-obj-data canvas-obj))
      ;; keep fixed number of elements to plot
      (trim-data env data-length-max)
      (let ((y0-mapped (map-y0-to-plot y0 (plot-env-y-min env) (plot-env-y-max env) plot-window-size))
	    (prev-point (cdadr (canvas-obj-data canvas-obj))))
 	(if prev-point ;; is the dataset larger then ((NIL . 0.0d0)) ?
	    (let ((prev-point-mapped (map-y0-to-plot prev-point (plot-env-y-min env) (plot-env-y-max env) plot-window-size)))
	      (draw-arc plot-window (canvas-obj-canvas canvas-obj) (- (- plot-window-size (/ point-size 2)) 2)
			(- y0-mapped (/ point-size 2)) point-size point-size 0 (* 2 pi) :fill-p)
	      (draw-line plot-window (canvas-obj-canvas canvas-obj) (- plot-window-size x-shift)
			 prev-point-mapped (- plot-window-size 2) y0-mapped))				     
	    (draw-arc plot-window (canvas-obj-canvas canvas-obj) (- (- plot-window-size (/ point-size 2)) 2)
		      (- y0-mapped (/ point-size 2)) point-size point-size 0 (* 2 pi) :fill-p)))
      ;; check if we need rescaling (bug here. see commit 0c04f5b)
      (when (recompute-y-limits y0 env)
	(redraw-plot-window env plot-window point-size plot-window-size grid y-coords dx-grid x-end t-max dt)))))
	

(defun draw-vertical-grid (env grid dx-grid x-shift plot-window plot-window-size)
  (if (>= (plot-env-grid-c env) dx-grid)
      (progn
	;; one pixel back to make the line visible
	(create-vertical-grid-lines plot-window grid plot-window-size (list (- plot-window-size 1)))
	(setf (plot-env-grid-c env) 0))
      (incf (plot-env-grid-c env) x-shift)))

(defun push-NIL-data (env data-length-max)
  (mapc #'(lambda (obj) (push NIL (canvas-obj-data obj))) (plot-env-canvas-obj-db env))  
  (trim-data env data-length-max))

(defun draw-y-min-max (env main-window text-layer text-layer-background font-ascent x-coord y-min-coord y-max-coord)
  (let ((y-min-string (format NIL "~10,3F" (plot-env-y-min env)))
	(y-max-string (format NIL "~10,3F" (plot-env-y-max env))))
    (draw-rectangle main-window text-layer-background x-coord (- y-max-coord font-ascent) (text-width text-layer y-max-string) font-ascent :fill-p)
    (draw-rectangle main-window text-layer-background x-coord (- y-min-coord font-ascent) (text-width text-layer y-min-string) font-ascent :fill-p)
    (draw-glyphs main-window text-layer x-coord y-min-coord y-min-string)
    (draw-glyphs main-window text-layer x-coord y-max-coord y-max-string)))

(defun draw-plot-title (main-window text-layer x-coord y-coord dt t-max)
  (draw-glyphs main-window text-layer x-coord y-coord (format NIL "dt = ~3,3F sec, t-max = ~3,1F sec" dt t-max)))
  
(defun plot-loop (n dt)
  "Create plotting environment, fetch plot-data (pd) from a data queue and plot it."
  (multiple-value-bind (display screen colormap) (make-default-display-screen-colormap)
    (multiple-value-bind (window-size x-start x-end plot-window-size) (make-plot-window 1500 0.1 0.5)
      (multiple-value-bind (main-window plot-window grid) (make-x11-layers screen window-size colormap x-start plot-window-size)
	(multiple-value-bind (text-layer-background text-layer font-ascent y-min-yc y-max-yc y-min-max-xc x-title y-title) (make-text-area main-window screen display colormap x-end x-start)
	  (unwind-protect
	       (let ((env (make-plot-env)) ;; the _env_ lexical environment is modified. The rest ist const.
		     (n-yticks 10)
		     (n-xticks 10)
		     (t-max 10)
		     (point-size 4))		 
		 (multiple-value-bind (x-coords dx-grid) (linspace 0 plot-window-size n-xticks)
		   (multiple-value-bind (y-coords x-shift data-length-max)
		       (init-coordinate-settings plot-window-size n-yticks t-max dt)		 
		     (draw-initial-grid display plot-window grid plot-window-size x-coords y-coords)
		     (draw-plot-title main-window text-layer x-title y-title dt t-max)
		     (loop :repeat n do
		       (draw-y-min-max env main-window text-layer text-layer-background font-ascent y-min-max-xc y-min-yc y-max-yc)
		       (draw-vertical-grid env grid dx-grid x-shift plot-window plot-window-size)		     		     
		       (let ((pd (sb-concurrency:dequeue *plot-queue*)))		      
			 ;; process data from queue
			 (if pd
			     (plot-pd pd env screen grid plot-window t-max dt x-end dx-grid point-size x-shift data-length-max plot-window-size y-coords colormap)
			     (push-NIL-data env data-length-max)))			   
		       (create-horizontal-grid-lines plot-window grid plot-window-size (- plot-window-size x-shift) y-coords)
		       (display-force-output display)
		       (sleep dt))))
		 (sleep 1))))
	(sleep 1)
	(display-finish-output display)
	(close-display display)))))

  (defun test (n dt)
    (loop for x = (sb-concurrency:dequeue *plot-queue*) do
      (unless x
	(return)))	
    ;;(can-logger.can2data::read-can-data)
    (can-logger.can2data::generate-data n)
    (plot-loop n dt))
