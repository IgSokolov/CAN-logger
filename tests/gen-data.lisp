(defpackage :can-logger.demo
  (:use :cl))

(in-package :can-logger.demo)

(defun get-scaling-coefficients (xmax xmin ymax ymin)
  "Mapping {xmin, xmax} -> {ymin, ymax}"
  (values
   (/ (- ymax ymin) (- xmax xmin) )
   (/ (- (* ymin xmax) (* ymax xmin)) (- xmax xmin))))

(defun i16-as-le-bytes (val)
  "Convert 16-bit signed integer to bytes (le)"
  (cons
   (ldb (byte 8 0) val)
   (ldb (byte 8 8) val)))
      
(defun float-to-16i-int (val)
  "Convert float to 16-bit signed int (for demo case)"
  (multiple-value-bind (a b) (get-scaling-coefficients 100 -100 32767 -32767)
    (round (+ (* val a) b))))

(defun make-payload (fn tmax dt)
  "Convert f(x) to bytes for CAN frame payload"
  (let ((t-points (can-logger.plotter::linspace-2 0 tmax dt))) ;; rm ::
    (mapcar #'(lambda (float-value) (i16-as-le-bytes (float-to-16i-int float-value)))
	    (mapcar fn t-points))))

(defun make-data-for-cangen-script (path can-interface can-id tmax freq dt)
  "Create a script which generates CAN frames"
  (with-open-file (f path :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (let ((test-fn-1 (lambda (x) (* x (sin (* 2 pi freq x)))))
	  (test-fn-2 (lambda (x) (* x (cos (* 2 pi freq x)))))
	  (delay 0.1))
      (let ((bytes-fn-1 (make-payload test-fn-1 tmax dt))
	    (bytes-fn-2 (make-payload test-fn-2 tmax dt)))
	(loop for bytes-1 in bytes-fn-1
	      for bytes-2 in bytes-fn-2
	      do
		 (write-sequence
		  (format NIL "cansend ~a ~a#~2,'0X~2,'0X~2,'0X~2,'0X~%" can-interface can-id
			  (car bytes-1) (cdr bytes-1) (car bytes-2) (cdr bytes-2)) f)
		 (write-sequence (format NIL "sleep ~,5f~%" delay) f))))))
		 
;; To create mock-up CAN data execute this in REPL:
;; (can-logger.demo::make-data-for-cangen-script "test-data" "vcan0" 485 10 0.5 0.01)
