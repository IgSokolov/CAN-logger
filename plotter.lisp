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

(defun time-offset (t0 t-max)
  (mod t0 t-max))

(defun map-y0-to-plot (y0 y-min y-max plot-window-size)
  (round (* plot-window-size (/ (- y-max y0) (- y-max y-min)))))

(defun compute-time (data-list plot-window-size t-max x-shift)
  (let ((x-start (round (* plot-window-size (/ (caar data-list) t-max))))
	(size (list-length data-list)))
    (loop for i from 0 below (- size 1)
	  collect (- x-start (* i x-shift)))))
		   
(defun reset-data (t-max dt data)
  (let ((i -1))
    (mapcar #'(lambda(point) (if point
				 (cons (- t-max (* (incf i) dt)) (cdr point))
				 (progn (incf i) NIL)))
	    data)))

(defun make-plot-buf (data-list plot-window-size t-max x-shift y-min y-max)
  (let ((x-coords (compute-time data-list plot-window-size t-max x-shift))
	(plot-buf))
    (format t "x-coords = ~a~%" x-coords)
    (loop for x in x-coords
	  for point in data-list
	  for y = (cdr point) do
	    (push (map-y0-to-plot y y-min y-max plot-window-size) plot-buf)
	    (push x plot-buf))
    plot-buf))

;; ((20.0 . 43) (19.9 . 42) (19.8 . 41) (19.7 . 40) (19.6 . 39)
;;               (19.5 . 38) (19.4 . 37) (19.3 . 36) (19.2 . 35) (19.1 . 34)
;;              (19.0 . 33) (18.9 . 32) (18.8 . 31) (18.7 . 30))
;; (670 257 674 242 678 227 681 212 685 197
;;                                      688 183 692 168 695 153 699 138 703 123
;;                                      706 108 710 94 713 79 717 64)

(defun correct-x-shift (cur prev x-shift-ref)
  (if (not (= x-shift-ref (- prev cur)))
      (- prev x-shift-ref)
      cur))

(defun correct-x-data (data-list x-shift-ref)
  (let ((prev (caar data-list)))
    (mapcar #'(lambda (point)
	      (prog1
		  (cons (correct-x-shift (car point) prev x-shift-ref)
			(cdr point))
		(setq prev (car point)))) data-list)))
						   
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

;; from P. Graham (On Lisp)
(defun flatten (x)
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun get-last-t0 (list-of-data-sets)
  (second (reverse (flatten list-of-data-sets)))) ;; fixme too much work?

(defun create-horizontal-grid-lines (win gcontext plot-window-size x-start y-coords)
  (mapc (lambda (y) (draw-line win gcontext x-start y plot-window-size y)) y-coords))

(defun create-vertical-grid-lines (win gcontext plot-window-size x-coords)
  ;;(assert (not (and x-start draw-last-line-only)))  
  (mapc (lambda (x) (draw-line win gcontext x 0 x plot-window-size)) x-coords))

;; (defun create-grid (win gcontext x-start x-end)
;;   (let* ((plot-window-size (- x-end x-start 1))
;; 	 (points (linspace 0 plot-window-size 10)))
;;     ;;(draw-rectangle win gcontext 0 0 plot-window-size plot-window-size)
;;     (mapc (lambda (x) (draw-line win gcontext x 0 x plot-window-size)) points)
;;     (mapc (lambda (x) (draw-line win gcontext 0 x plot-window-size x)) points)))

(defun plot-loop (n dt)
  (let* ((display (open-default-display))
         (screen (display-default-screen display))
         (colormap (screen-default-colormap screen)))
    (let* ((window-size 1024)
	   (x-start (round (* 0.1 window-size)))
	   (x-end (round (* 0.8 window-size)))
	   (plot-window-size (- x-end x-start)))
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
			   :background (alloc-color colormap(lookup-color colormap "black"))))
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
	(unwind-protect
	     (progn
	       (let ((data)
		     (t-max 20)
		     (y-min -1)
		     (y-max 1)
		     (c 0)
		     (l 0)
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
		       (format t "time elapsed = ~a~%" (* dt (incf c)))
		       (copy-area plot-window canvas x-shift 0 plot-window-size plot-window-size plot-window 0 0)		       
		       (let ((pd (sb-concurrency:dequeue *plot-queue*)))
			 ;; draw grid
			 ;; horizontal			 
			 (if (>= grid-c dx-grid)
			     (progn			       
			       (create-vertical-grid-lines plot-window grid plot-window-size (list (- plot-window-size 1)))
			       (setq grid-c 0))
			     (incf grid-c x-shift))
			 
			 (if pd			   
			     (let ((y0 (plot-data-y pd)))				 	 
			       (push (cons NIL y0) data)			     
			       (if (= data-counter data-length-max)			     			     
				   (setq data (butlast data))			     			       
				   (incf data-counter))			     

 			       (when (cdadr data)
				 (let ((y0-mapped (map-y0-to-plot y0 y-min y-max plot-window-size))
				       (prev-point (map-y0-to-plot (cdadr data) y-min y-max plot-window-size)))
				   (draw-line plot-window canvas (- plot-window-size x-shift) prev-point (- plot-window-size 1) y0-mapped)))
			       
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
				     ;;(format t "data = ~a~%" data)
				     (let* ((data (reset-data t-max dt data))
					    (data-list (split-data data)))
				       (format t "data list = ~a =>~%" data-list)
				       ;; (dolist (dl data-list)					
				       ;; 	 (let ((pb (make-plot-buf dl plot-window-size t-max x-shift y-min y-max)))
				       ;; 	   (format t "new buf = ~a~%" pb)
				       ;; 	   (draw-lines plot-window canvas pb)))
				       				       
				       ;;(format t "data-list = ~a => " data-list) ;; dt = 0.1 strictly. 
				       ;;(format t "reset-data = ~a~%" data)
				       ;;(format t "last = ~a~%" (get-last-t0 data-list))
				       ;;(format t "start x-grid = ~a~%" (- dx-grid grid-c))
				       ;;(format t "dx-grid = ~a, grid-c = ~a, x-end = ~a. x-shit = ~a~%" dx-grid grid-c x-end x-shift)
				       
				       (dolist (data-item data-list) 				       				       
					 (let ((plot-buf))					   
					   (dolist (item data-item)
					     (destructuring-bind (t0 . y0) item					       
					       (let ((y0-mapped (map-y0-to-plot y0 y-min y-max plot-window-size)))
						 ;;(format t "t0 = ~a, (* plot-window-size (/ t0 t-max)) = ~a, x0 = ~a~%" t0 (* plot-window-size (/ t0 t-max)) (round (* plot-window-size (/ t0 t-max))))
						 (push y0-mapped plot-buf)
						 (push (round (* plot-window-size (/ t0 t-max))) plot-buf)))
					     (format t "buf = ~a~%" plot-buf)		     
					     (draw-lines plot-window canvas plot-buf))))

				       )))))
			     (progn			     
			       (push NIL data)
			       (if (= data-counter data-length-max)			     			     
				   (setq data (butlast data))			     			       
				   (incf data-counter))))
			 (create-horizontal-grid-lines plot-window grid plot-window-size (- plot-window-size x-shift) y-coords)
			 (display-force-output display)
			 (sleep dt))))))))
	(sleep 1)))
    (sleep 1)
    (display-finish-output display)
    (CLOSE-DISPLAY display)))

;; (9.9 .. 0.1)
;; (6.9 ..  3.1) -> (0 .. 10)

(defun test (n dt)
  (loop for x = (sb-concurrency:dequeue *plot-queue*) do
    (unless x
      (return)))	
  ;;(can-logger.can2data::read-can-data)
  (can-logger.can2data::generate-data n)
  (plot-loop n dt))
  
;; wrong y before/after redraw
;; update lot wrong point
