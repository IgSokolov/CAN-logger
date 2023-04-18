(defpackage :can-logger.demo
  (:use :cl))

(in-package :can-logger.demo)

(defun i16-as-le-bytes (val)
  "Convert 16-bit signed integer to bytes (le)"
  (cons
   (ldb (byte 8 0) val)
   (ldb (byte 8 8) val)))
      
(defun float-to-16i-int (raw-val bit-fact p-fact p-offset)
  "Convert float to 16-bit signed int (for demo case)"
  (let ((val (/ (- raw-val p-offset) p-fact bit-fact)))
    (cond
      ((> val 32767) 32767)
      ((< val -32767) -32767)
      (t (round val)))))

(defun make-payload (fn tmax dt)
  "Convert f(x) to bytes for CAN frame payload"
  (let ((t-points (can-logger.plotter::linspace-2 0 tmax dt)))
    (mapcar #'(lambda (float-value) (i16-as-le-bytes (float-to-16i-int float-value 0.01 1 0.5)))
	    (mapcar fn t-points))))

(defun make-data-for-cangen-script (path can-interface can-id-1 can-id-2 tmax freq dt)
  "Create a script which generates CAN frames"
  (with-open-file (f path :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (let (;;(test-fn-1 (lambda (x) (* x (sin (* 2 pi freq x)))))
	  (test-fn-1 (lambda (x) (sin (* 2 pi freq x))))
	  (test-fn-2 (lambda (x) (cos (* 2 pi freq x))))
	  (test-fn-3 (lambda (x) (sin (* 2 pi (* 2 freq) x))))
	  (test-fn-4 (lambda (x) (cos (* 2 pi (* 2 freq) x))))	  
	  (delay (- 0.1 0.018)))
      (let ((bytes-fn-1 (make-payload test-fn-1 tmax dt))
	    (bytes-fn-2 (make-payload test-fn-2 tmax dt))
	    (bytes-fn-3 (make-payload test-fn-3 tmax dt))
	    (bytes-fn-4 (make-payload test-fn-4 tmax dt)))
	(loop for bytes-1 in bytes-fn-1
	      for bytes-2 in bytes-fn-2
	      for bytes-3 in bytes-fn-3
	      for bytes-4 in bytes-fn-4
	      do
		 (write-sequence
		  (format NIL "cansend ~a ~a#~2,'0X~2,'0X~2,'0X~2,'0X~%"
			  can-interface can-id-1
			  (car bytes-1) (cdr bytes-1) (car bytes-2) (cdr bytes-2)) f)
		 (write-sequence
		  (format NIL "cansend ~a ~a#~2,'0X~2,'0X~2,'0X~2,'0X~%"
			  can-interface can-id-2
			  (car bytes-3) (cdr bytes-3) (car bytes-4) (cdr bytes-4)) f)		 
		 (write-sequence (format NIL "sleep ~,5f~%" delay) f)
	      )))))
		 
;; To create mock-up CAN data execute this in REPL:
;;(can-logger.demo::make-data-for-cangen-script "test-data" "vcan0" 485 10 0.5 0.01)
