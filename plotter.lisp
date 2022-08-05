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
                :foreground (alloc-color colormap (lookup-color colormap "orange"))))
	 (canvas (create-gcontext
                  :drawable plot-window
                  :fill-style :solid
                  :background (screen-white-pixel screen)
                  :foreground (alloc-color colormap (lookup-color colormap "green")))))
    (map-window main-window)
    (map-window plot-window)
    (values plot-window grid canvas)))
    
(defun plot-loop (n dt)
  (multiple-value-bind (display screen colormap) (make-default-display-screen-colormap)
    (multiple-value-bind (window-size x-start x-end plot-window-size) (make-plot-window 1024 0.1 0.8)
      (multiple-value-bind (plot-window grid canvas) (make-x11-layers screen window-size colormap x-start plot-window-size)
	(unwind-protect
	     (let ((data)
		   (t-max 10)
		   (y-min -1)
		   (y-max 1)
		   (n-yticks 10)
		   (n-xticks 10)
		   (plot-window plot-window))
	       (multiple-value-bind (x-coords dx-grid) (linspace 0 plot-window-size n-xticks)
		 ;;(setq x-coords (cdr x-coords))		   
		 (let ((y-coords (cdr (linspace 0 plot-window-size n-yticks)))			 
		       (grid-c 0)		     
		       (data-length-max (round (/ t-max dt)))
		       (data-counter 0)
		       (x-shift (round (* plot-window-size (/ dt t-max)))))
		   
		   (loop :repeat 2 do
		     (create-horizontal-grid-lines plot-window grid plot-window-size 0 y-coords)
		     (create-vertical-grid-lines plot-window grid plot-window-size (append x-coords (list (- plot-window-size 1))))
		     (display-force-output display)
		     (sleep 0.1))

		   (loop :repeat n do
		     ;;(format t "time elapsed = ~a~%" (* dt (incf c)))
		     (copy-area plot-window canvas x-shift 0 plot-window-size plot-window-size plot-window 0 0)		       
		     (let ((pd (sb-concurrency:dequeue *plot-queue*)))
		       ;; draw horizontal grid
		       (if (>= grid-c dx-grid)
			   (progn			       
			     (create-vertical-grid-lines plot-window grid plot-window-size (list (- plot-window-size 1)))
			     (setq grid-c 0))
			   (incf grid-c x-shift))
		       ;; process data from queue
		       (if pd  
			   (let ((y0 (plot-data-y pd)))				 	 
			     (push (cons NIL y0) data)
			     ;; keep fixed number of elements to plot
			     (if (= data-counter data-length-max)			     			     
				 (setq data (butlast data))			     			       
				 (incf data-counter))			     
			     ;; plot if possible
 			     (when (cdadr data)
			       (let ((y0-mapped (map-y0-to-plot y0 y-min y-max plot-window-size))
				     (prev-point (map-y0-to-plot (cdadr data) y-min y-max plot-window-size)))
				 (draw-line plot-window canvas (- plot-window-size x-shift) prev-point (- plot-window-size 1) y0-mapped)))
			     ;; check if we need rescaling
			     (multiple-value-bind (new-min new-max) (recompute-y-limits y0 y-min y-max (cdr data))
			       (when (or (not (= new-min y-min))
					 (not (= new-max y-max)))
				 (progn ;; redraw everything
				   (clear-area plot-window)
				   ;; redraw grid				     
				   (create-horizontal-grid-lines plot-window grid plot-window-size 0 y-coords)
				   (create-vertical-grid-lines plot-window grid plot-window-size (linspace-2 (- dx-grid grid-c) x-end dx-grid))
				   (setq y-min new-min
					 y-max new-max)
				   (let* ((data (reset-data t-max dt data))
					  (data-list (split-data data)))				       				       				       
				     (dolist (data-item data-list) 				       				       
				       (let ((plot-buf)					 
					     (y0-mapped-list (mapcar #'(lambda (item) (map-y0-to-plot (cdr item) y-min y-max plot-window-size)) data-item))
					     (t0-mapped-list (mapcar #'(lambda (item) (round (* plot-window-size (/ (car item) t-max)))) data-item)))
					 (loop for y0 in y0-mapped-list
					       for t0 in t0-mapped-list do
						 (push y0 plot-buf)						 
					    	 (push t0 plot-buf))
					 (draw-lines plot-window canvas plot-buf))))))))				     
			   (progn			     
			     (push NIL data)
			     (if (= data-counter data-length-max)			     			     
				 (setq data (butlast data))			     			       
				 (incf data-counter))))
		       (create-horizontal-grid-lines plot-window grid plot-window-size (- plot-window-size x-shift) y-coords)
		       (display-force-output display)
		       (sleep dt)))))))
	(sleep 1)))
    (sleep 1)
    (display-finish-output display)
    (CLOSE-DISPLAY display)))

(defun test (n dt)
  (loop for x = (sb-concurrency:dequeue *plot-queue*) do
    (unless x
      (return)))	
  ;;(can-logger.can2data::read-can-data)
  (can-logger.can2data::generate-data n)
  (plot-loop n dt))
  
;; wrong y before/after redraw
;; update lot wrong point
