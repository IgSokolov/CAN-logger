(in-package :can-logger.main)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency))

(defun make-default-display-screen-colormap ()
  (let* ((display (open-default-display))
         (screen (display-default-screen display))
         (colormap (screen-default-colormap screen)))
    (values display screen colormap)))

(defparameter *plot-queue* (sb-concurrency:make-queue :initial-contents NIL))
(defparameter *stop* NIL)

(defun empty-queue (queue)  
  (loop for data = (sb-concurrency:dequeue queue) do
    (unless data
      (return))))

(defun stop-gui ()
  (setq *stop* t)
  (stop-reading-CAN-data)
  (close-widget-plot))

(defun show-widget-header (top-level-window top-level-gcontext widget-window header)
  (let ((x0 (drawable-x widget-window))
	(y0 (drawable-y widget-window))
	(width (drawable-width widget-window))
	(header-width (text-width (gcontext-font top-level-gcontext) header))
	(font-height (font-ascent (gcontext-font top-level-gcontext))))
    (let ((xc (+ x0 (round (/ (- width header-width) 2))))
	  (yc (- y0 font-height 5)))      
      (draw-glyphs top-level-window top-level-gcontext xc yc header))))

(defun run (&optional config-file can-interface dt t-max)
  (let ((config-file config-file)
	(can-interface can-interface)
	(dt dt)
	(t-max t-max))
    (unless (and config-file can-interface dt t-max)
      (multiple-value-bind (config-file-cli can-interface-cli dt-cli t-max-cli) (parse-cli-args)
	(setq config-file config-file-cli
	      can-interface can-interface-cli
	      dt (/ dt-cli 1000)
	      t-max t-max-cli)))
    (format t "Running CAN logger:~%config path = ~a~%can interface = ~a~%dt = ~f s~%t-max = ~a s~%" config-file can-interface dt t-max)      
    (setq *stop* NIL)

    (handler-case
	(let ((can-db-obj (make-can-db-obj :db (make-can-db config-file))))
	  (mapc #'empty-queue (list *plot-queue*))
	  (sb-thread:make-thread (lambda () (read-can-data :can-interface can-interface
							   :can-db-obj can-db-obj
							   :output-queues-analog (list *plot-queue*))))
	  (sleep 0.1)
	  (multiple-value-bind (display screen colormap) (make-default-display-screen-colormap)
	    (let* ((top-level  (create-window
				:parent (screen-root screen)
				:x 0
				:y 0
				:width 850
				:height 850
				:border (screen-black-pixel screen)
				:border-width 2
				:bit-gravity :center
				:colormap colormap				
				:background (alloc-color colormap
							 (lookup-color colormap "gray"))))
		   (top-level-gcontext (create-gcontext
					:drawable top-level
					:font (open-font display "-bitstream-bitstream charter-bold-r-normal--0-0-0-0-p-0-iso8859-1")
					:line-style :solid
					:background (screen-white-pixel screen)
					:foreground (alloc-color colormap
								 (lookup-color colormap "black"))))
		   (plot-window (cons "CAN BUS Monitor"
				      (create-window
				       :parent top-level
				       :x 0
				       :y 50
				       :width 800
				       :height 750
				       :border (screen-black-pixel screen)
				       :border-width 2
				       :bit-gravity :center
				       :colormap colormap				       
				       :background (alloc-color colormap (lookup-color colormap "gray"))))))
	      (map-window top-level)
	      (map-subwindows top-level)
	      (mapc #'(lambda (obj)
			(show-widget-header top-level top-level-gcontext (cdr obj) (car obj))
			(sleep 0.1) ;; without sleep clx doesn't produce output
			(display-force-output display))		 
		    (list plot-window))            
	      (unwind-protect ;; todo handle SB-INT:BROKEN-PIPE and END-OF-FILE and SB-INT:SIMPLE-STREAM-ERROR		   
		   (sb-thread:join-thread
		    (sb-thread:make-thread (lambda () (make-widget-plot
						      :main-window (cdr plot-window)
						      :display display
						      :screen screen
						      :colormap colormap
						      :x-start 0 :y-start 0 :size 800
						      :data-queue *plot-queue* :t-max t-max :dt dt)))) ;; todo: dt + t_max -> cli!
		(stop-gui)
		(close-display display)))))
      (sb-sys:interactive-interrupt () (stop-gui)))))
