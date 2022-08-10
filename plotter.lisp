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
  (let ((i -1))
    (mapcar #'(lambda(point) (if point
				 (cons (- t-max (* (incf i) dt)) (cdr point))
				 (progn (incf i) NIL)))
	    data)))
						   
(defun recompute-y-limits (y0 y-min y-max data)
  (if data
      (multiple-value-bind (smallest biggest) (loop for x in data
						    when x
						      minimize (cdr x) into min
						    when x
						      maximize (cdr x) into max
						    finally (return (values min max)))
	;;(format t "smallest = ~a, biggest = ~a~%" smallest biggest)
	(values
	 (if (< y0 smallest)       
	     (coerce (- y0 (* (abs y0) 0.1)) 'short-float)
	     y-min)
	 (if (> y0 biggest)       
	     (coerce (+ y0 (* (abs y0) 0.1)) 'short-float)
	     y-max)))
      (values y-min y-max)))

(defun split-data (data)
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
  (let* ((x-start (round (* start-offest-in-% size)))
	 (x-end (round (* end-offset-in-% size)))
	 (plot-window-size (- x-end x-start)))
    (values size x-start x-end plot-window-size)))

(defun make-x11-layers (screen window-size colormap x-start plot-window-size)
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
    (values plot-window grid)))

(defstruct canvas-obj
  canvas
  label
  data)

(defun make-canvas (plot-window screen colormap color)
  (create-gcontext
   :drawable plot-window
   :fill-style :solid
   :background (screen-white-pixel screen)
   :foreground (alloc-color colormap (lookup-color colormap color))))

(defun add-canvas-obj (db label plot-window screen colormap color)
  (let ((obj (make-canvas-obj
	      :canvas (make-canvas plot-window screen colormap color)
	      :label label)))
    (push obj db)
    (values obj db)))

(defun fetch-canvas-obj (label db)
  (let ((canvas-obj (find label db :test #'string= :key #'canvas-obj-label)))
    (values
     canvas-obj
     (remove canvas-obj db))))

(defun draw-initial-grid (display plot-window grid plot-window-size x-coords y-coords)
  (loop :repeat 2 do ;; 2 because otherwise no result obtained from x11 server. a bug?
    (create-horizontal-grid-lines plot-window grid plot-window-size 0 y-coords)
    (create-vertical-grid-lines plot-window grid plot-window-size (append x-coords (list (- plot-window-size 1))))
    (display-force-output display)
    (sleep 0.1)))

(defun init-coordinate-settings (plot-window-size n-yticks t-max dt)
   (let ((y-coords (cdr (linspace 0 plot-window-size n-yticks)))			 
	 (grid-c 0)		     
	 (data-length-max (round (/ t-max dt)))
	 (data-counter 0)
	 (x-shift (round (* plot-window-size (/ dt t-max)))))
     (values y-coords x-shift data-length-max grid-c data-counter)))

(defun plot-loop (n dt)
  (multiple-value-bind (display screen colormap) (make-default-display-screen-colormap)
    (multiple-value-bind (window-size x-start x-end plot-window-size) (make-plot-window 1500 0.1 0.5)
      (multiple-value-bind (plot-window grid) (make-x11-layers screen window-size colormap x-start plot-window-size)
	(unwind-protect
	     (let ((canvas-obj-db)
		   (list-of-colors (list "green" "red"))
		   (t-max 10)
		   (y-min -1)
		   (y-max 1)
		   (n-yticks 10)
		   (n-xticks 10)
		   (plot-window plot-window)) ;; shared between canvas-objs
	       (multiple-value-bind (x-coords dx-grid) (linspace 0 plot-window-size n-xticks)
		 ;;(setq x-coords (cdr x-coords))
		 (multiple-value-bind (y-coords x-shift data-length-max grid-c data-counter) (init-coordinate-settings plot-window-size n-yticks t-max dt)		 
		   (draw-initial-grid display plot-window grid plot-window-size x-coords y-coords)		   		   
		   (loop :repeat n do
		     ;; draw horizontal grid
		     (if (>= grid-c dx-grid)
			 (progn
			   ;; one pixel back to make the line visible
			   (create-vertical-grid-lines plot-window grid plot-window-size (list (- plot-window-size 1)))
			   (setq grid-c 0))
			 (incf grid-c x-shift))
		     ;;(format t "time elapsed = ~a~%" (* dt (incf c)))		     
		     (let ((pd (sb-concurrency:dequeue *plot-queue*)))		      
		       ;; process data from queue
		       (if pd  
			   (let ((y0 (plot-data-y pd))
				 (label (plot-data-label pd)))			     
			     (multiple-value-bind (canvas-obj rest-canvas-objs) (fetch-canvas-obj label canvas-obj-db)			       
			       ;; push NIL-data to the rest of canvasses
			       (mapc #'(lambda (obj) (push NIL (canvas-obj-data obj))) rest-canvas-objs)			       
			       (unless canvas-obj
				 (multiple-value-bind (new-canvas-obj new-db) (add-canvas-obj canvas-obj-db label plot-window screen colormap (pop list-of-colors))
				   (setq canvas-obj new-canvas-obj
					 canvas-obj-db new-db)))
			       (mapc #'(lambda (canvas-obj) ;; or just one? check it! 
					 (copy-area plot-window (canvas-obj-canvas canvas-obj) x-shift 0 plot-window-size plot-window-size plot-window 0 0))
				     (list canvas-obj))    			       
			       ;; we consing NIL because we dont need absolute timestamp 99.9 % of time.
			       ;; Only if we need to rescale and redraw the plot, NILs are filled with time values,
			       ;; which are computed at every redraw cycle.
			       (push (cons NIL y0) (canvas-obj-data canvas-obj)) 
			       ;; keep fixed number of elements to plot
			       (if (= data-counter data-length-max)
				   (mapc #'(lambda (obj) (setf (canvas-obj-data obj) (butlast (canvas-obj-data obj)))) canvas-obj-db)
				   (incf data-counter))
			       ;;(format t "data = ~a~%" (canvas-obj-data canvas-obj))
			       (let ((y0-mapped (map-y0-to-plot y0 y-min y-max plot-window-size))
				     (prev-point (cdadr (canvas-obj-data canvas-obj))))
 				 (if prev-point ;; is the dataset larger then ((NIL . 0.0d0)) ?
				     (let ((prev-point-mapped (map-y0-to-plot prev-point y-min y-max plot-window-size)))
				       (draw-arc plot-window (canvas-obj-canvas canvas-obj) (- (- plot-window-size 2) 2) (- y0-mapped 2) 4 4 0 (* 2 pi) :fill-p)
				       (draw-line plot-window (canvas-obj-canvas canvas-obj) (- plot-window-size x-shift) prev-point-mapped (- plot-window-size 2) y0-mapped))				     
				     (draw-arc plot-window (canvas-obj-canvas canvas-obj) (- (- plot-window-size 2) 2) (- y0-mapped 2) 4 4 0 (* 2 pi) :fill-p)))				     
				   
			       ;; check if we need rescaling (bug here. see commit 0c04f5b)
			       (multiple-value-bind (new-min new-max) (recompute-y-limits y0 y-min y-max (cdr (canvas-obj-data canvas-obj)))
				 (when (or (not (= new-min y-min))
					   (not (= new-max y-max)))
				   (progn ;; redraw everything				     
				     (clear-area plot-window)
 				     ;; redraw grid				     
				     (create-horizontal-grid-lines plot-window grid plot-window-size 0 y-coords)
				     (create-vertical-grid-lines plot-window grid plot-window-size (linspace-2 (- dx-grid grid-c) x-end dx-grid))
				     (setq y-min new-min
					   y-max new-max)
				     ;; redraw data
				     (dolist (canvas-obj canvas-obj-db)
				       (let* ((data (reset-data t-max dt (canvas-obj-data canvas-obj)))
					      (data-list (split-data data)))
					 ;;(format t "obj = ~a~%data = ~a~%data-list = ~a~%" canvas-obj data data-list)
					 (dolist (data-item data-list)
					   ;;(format t "data item = ~a~%" data-item)
					   (let ((plot-buf-1)
						 (plot-buf-2)
						 (y0-mapped-list (mapcar #'(lambda (item) (map-y0-to-plot (cdr item) y-min y-max plot-window-size)) data-item))
						 (t0-mapped-list (mapcar #'(lambda (item) (round (* plot-window-size (/ (car item) t-max)))) data-item)))
					     (loop for y0 in y0-mapped-list
						   for t0 in t0-mapped-list do
						     (push y0 plot-buf-1)
						     (push t0 plot-buf-1)
						     
						     (push (* 2 pi) plot-buf-2)
						     (push 0 plot-buf-2)
						     (push 4 plot-buf-2)
						     (push 4 plot-buf-2)						     						     
						     (push (- y0 2) plot-buf-2)
						     (push (- t0 2) plot-buf-2)						     						     
						   )
					     (if (> (list-length plot-buf-1) 2) ;; cdadr ?
						 (progn
						   (draw-lines plot-window (canvas-obj-canvas canvas-obj) plot-buf-1)
						   (draw-arcs plot-window (canvas-obj-canvas canvas-obj) plot-buf-2 :fill-p))						   
						 (draw-arc plot-window (canvas-obj-canvas canvas-obj) (- (first plot-buf-1) 2) (- (second plot-buf-1) 2) 4 4 0 (* 2 pi) :fill-p)
						 ))))))))
			       

			       ))
			   (progn			     
			     (mapc #'(lambda (obj) (push NIL (canvas-obj-data obj))) canvas-obj-db)
			     (if (= data-counter data-length-max)
				 (mapc #'(lambda (obj) (setf (canvas-obj-data obj) (butlast (canvas-obj-data obj)))) canvas-obj-db)
				 (incf data-counter))))
		       (create-horizontal-grid-lines plot-window grid plot-window-size (- plot-window-size x-shift) y-coords)
		       (display-force-output display)
		       (sleep dt)))
		   (sleep 1))))
	  (sleep 1)
	  (display-finish-output display)
	  (CLOSE-DISPLAY display))))))

(defun test (n dt)
  (loop for x = (sb-concurrency:dequeue *plot-queue*) do
    (unless x
      (return)))	
  ;;(can-logger.can2data::read-can-data)
  (can-logger.can2data::generate-data n)
  (plot-loop n dt))
