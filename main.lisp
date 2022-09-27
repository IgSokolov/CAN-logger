(in-package :can-logger.main)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency))

(defun make-default-display-screen-colormap ()
  (let* ((display (open-default-display))
         (screen (display-default-screen display))
         (colormap (screen-default-colormap screen)))
    (values display screen colormap)))

(defparameter *data-queue-1* (sb-concurrency:make-queue :initial-contents NIL)) ;; todo rename.
(defparameter *data-queue-2* (sb-concurrency:make-queue :initial-contents NIL))
(defparameter *data-queue-3* (sb-concurrency:make-queue :initial-contents NIL))
(defparameter *button-task-queue* (sb-concurrency:make-queue :initial-contents NIL))

(defparameter *stop* NIL)

(defun multicast (value queues)
  (mapc #'(lambda (queue) (sb-concurrency:enqueue value queue)) queues))

(defun empty-queue (queue)
  (loop for data = (sb-concurrency:dequeue queue) do
    (unless data
      (return))))

(defun generate-data ()
  (let ((switch t)
	(i 0))
    (loop until *stop* do
      (let ((plot-value-1 (make-plot-data
			   ;;:value (* i (sin (* 2 pi 0.01 i)))
			   :value (+ 10 (random 10))
			   ;;:value 0.5
			   :can-id #x112
			   :label "label-1"))
	    (plot-value-2 (make-plot-data
			   ;;:value (* i (sin (* 2 pi 0.01 i)))
			   :value (+ 40 (random 10))
			   ;;:value 0.5
			   :can-id #x113
			   :label "label-2")))
	(when (= 0 (mod i 10))
	  (setq switch (not switch)))
	(if switch
	    ;;(sb-concurrency:enqueue NIL *plot-queue*)
	    (progn
	      (multicast plot-value-1 (list *data-queue-1* *data-queue-2*))
	      (sb-concurrency:enqueue (plot-data-can-id plot-value-1) *data-queue-3*))
	    (progn
	      (multicast plot-value-2 (list *data-queue-1* *data-queue-2*))
	      (sb-concurrency:enqueue (plot-data-can-id plot-value-2) *data-queue-3*))))
      (incf i)
      (sleep 0.1))))

(defun stop-gui ()
  (setq *stop* t)
  (stop-reading-CAN-data)
  (close-widget-table)
  (close-widget-plot)
  (close-widget-tiles))

(defun run-demo ()
  (setq *stop* NIL)
  (mapc #'empty-queue (list *data-queue-1* *data-queue-2* *data-queue-3* *button-task-queue*))
  ;;(sb-thread:make-thread (lambda () (read-can-data "vcan0" (list *data-queue-1* *data-queue-2*)))) ;; uncomment to read CAN bus
  (sb-thread:make-thread (lambda () (generate-data))) ;; comment to generate test data
  (sleep 0.1)
  (multiple-value-bind (display screen colormap) (make-default-display-screen-colormap)
    (let* ((top-level (create-window
		       :parent (screen-root screen)
		       :x 0
		       :y 0
		       :width (screen-width screen)
		       :height (screen-height screen)
		       :border (screen-black-pixel screen)
		       :border-width 2
		       :bit-gravity :center
		       :colormap colormap
		       :background (alloc-color colormap (lookup-color colormap "gray"))))
	   (plot-window (create-window
			 :parent top-level
			 :x 0
			 :y 50
			 :width 800
			 :height 800
			 :border (screen-black-pixel screen)
			 :border-width 2
			 :bit-gravity :center
			 :colormap colormap
			 :background (alloc-color colormap (lookup-color colormap "gray"))))
	   (table-window (create-window
			  :parent top-level
			  :x (round (* (screen-width screen) 0.5))
			  :y 50
			  :width 410
			  :height 800
			  :border (screen-black-pixel screen)
			  :border-width 2
			  :bit-gravity :center
			  :colormap colormap
			  :background (alloc-color colormap (lookup-color colormap "gray"))))
	   (tile-window (create-window
			  :parent top-level
			  :x (round (* (screen-width screen) 0.8))
			  :y 50
			  :width 320
			  :height 400
			  :border (screen-black-pixel screen)
			  :border-width 2
			  :bit-gravity :center
			  :colormap colormap
			  :background (alloc-color colormap (lookup-color colormap "gray")))))
      (map-window top-level)
      (map-window plot-window)
      (map-window table-window)
      (map-window tile-window)
      (unwind-protect
	   (progn	     
	     (sb-thread:make-thread (lambda () (make-widget-plot
						:main-window plot-window
						:display display
						:screen screen
						:colormap colormap
						:x-start 0 :y-start 0 :size 800
						:data-queue *data-queue-1* :dt 0.1)))
	     (sb-thread:make-thread (lambda () (make-widget-table
						:main-window table-window
						:display display
						:screen screen
						:colormap colormap
						:data-queue *data-queue-2*
						:x-table 0 :y-table 0
						:width 360 :height 800 :n-rows 20
						:x-buttons 360
						:y-buttons 0)))
	     (sb-thread:make-thread (lambda () (make-widget-tiles
						:main-window tile-window
						:display display
						:screen screen
						:colormap colormap
						:data-queue *data-queue-3*)))
	     (sleep 60)
	     (stop-gui))	
	;; (sb-thread:make-thread (lambda () (make-widget-button :main-window main-window :display display :screen screen
	;; 							   :label "STOP" :task-queue *button-task-queue*
	;; 							   :colormap colormap :x 1000 :y 500 :width 50 :height 50)))
	
	;; (loop for stop = (sb-concurrency:dequeue *button-task-queue*) do	       
	;;   (if stop
	;; 	   (progn		     
	;; 	     (stop-gui)
	;; 	     (return))
	;; 	   (sleep 1)))	     
	;; (close-display display))
	
	(stop-gui)
	(close-display display)))))
