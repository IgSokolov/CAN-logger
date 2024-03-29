(in-package :can-logger.can2data)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency))

(defstruct plot-data value label can-id)

(defun bytes-to-integer (bytes type endiannes)
  "Converts byte array to integer"
  (flet ((read-u8 (bytes)
	   (let ((val 0))
	     (setf (ldb (byte 8 0) val) (aref bytes 0))
	     val))
	 (read-u16 (bytes)
	   (let ((val 0))
	     (setf (ldb (byte 8 8) val) (aref bytes 0))
	     (setf (ldb (byte 8 0) val) (aref bytes 1))
	     val))
	 (read-u32 (bytes)
	   (let ((val 0))
	     (setf (ldb (byte 8 24) val) (aref bytes 0))
	     (setf (ldb (byte 8 16) val) (aref bytes 1))
	     (setf (ldb (byte 8 8) val) (aref bytes 2))
	     (setf (ldb (byte 8 0) val) (aref bytes 3))
	     val))
	 (unsigned-to-signed (val size)
	   (if (>= val (ash 1 (1- (* 8 size))))
	       (- val (ash 1 (* 8 size)))
	       val)))
    (ccase type
      (:u8
       (ccase endiannes
	 (:be (read-u8 bytes))
	 (:le (read-u8 (reverse bytes)))))
      (:i8
       (ccase endiannes
	 (:be (unsigned-to-signed (read-u8 bytes) 1))
	 (:le (unsigned-to-signed (read-u8 (reverse bytes)) 1))))
      (:u16
       (ccase endiannes
	 (:be (read-u16 bytes))
	 (:le (read-u16 (reverse bytes)))))
      (:i16
       (ccase endiannes
	 (:be (unsigned-to-signed (read-u16 bytes) 2))
	 (:le (unsigned-to-signed (read-u16 (reverse bytes)) 2))))
      (:u32
       (ccase endiannes
	 (:be (read-u32 bytes))
	 (:le (read-u32 (reverse bytes)))))
      (:i32
       (ccase endiannes
	 (:be (unsigned-to-signed (read-u32 bytes) 4))
	 (:le (unsigned-to-signed (read-u32 (reverse bytes)) 4)))))))

(defun pick-bytes (buffer data-type)
  "Picks first N bytes from buffer. Value of N depends
   on data-type."
  (let ((n 0))
    (ccase data-type
      ((or :u8 :i8) (setq n 1))
      ((or :u16 :i16) (setq n 2))
      ((or :u32 :i32) (setq n 4)))
    (values (subseq buffer 0 n) (subseq buffer n))))

;; idea from https://www.lispforum.com/viewtopic.php?t=1205
(defun byte-to-bits (n)
  "Convert byte to list with bits.
   Ex.: #xF8 -> '(1 1 1 1 1 0 0 0)"
  (labels ((rec (n)
	     (multiple-value-bind (q r) (floor n 2)
	       (if (and (zerop q) (zerop r)) nil
		   (cons r (rec q))))))
    (let ((bit-list (rec n)))
      (append (make-list (- 8 (list-length bit-list)) :initial-element 0) (reverse bit-list)))))

(defun make-plot-timestamp (can-timestamp)
  (destructuring-bind (t-sec t-usec) can-timestamp
    (float (+ t-sec (/ t-usec 1000000))))) ;; full-time

(defun process-can-frame (can-frame xnet-db output-queues-analog)
  "decode data and send it to plotter"
  (multiple-value-bind (can-id data timestamp origin) (parse-can-packet can-frame)
    (declare (ignore timestamp origin))
    ;;(format t "(do we need it ?) origin = ~a~%" origin)
    (if xnet-db
	(let ((xnet-item (gethash can-id xnet-db)))      
	  (if xnet-item	
	      (with-accessors ((signal-type signal-type)
			       (endiannes endiannes)
			       (data-type-mask data-type-mask)			 
			       (bit-factor-mask bit-factor-mask)
			       (physical-factor-mask physical-factor-mask)
			       (physical-offset-mask physical-offset-mask)
			       (label label)
			       (payload-size payload-size)
			       (multiplexed-p multiplexed-p)) xnet-item
		;; check if can-frame matches xnet layout
		(if (= payload-size (length data))
		    ;; do processing
		    (case signal-type
		      (:analog ;; float values (pressure, temperature, etc.)
		       (loop for data-type in data-type-mask		
			     for bit-factor in bit-factor-mask
			     for physical-factor in physical-factor-mask
			     for physical-offset in physical-offset-mask
			     for l in label do
			       (multiple-value-bind (bytes rest) (pick-bytes data data-type)		   
				 (let ((plot-value (make-plot-data
						    :value (+ physical-offset
							      (* bit-factor
								 physical-factor
								 (bytes-to-integer bytes data-type endiannes)))
						    :can-id can-id
						    :label l)))
				   (multicast plot-value output-queues-analog))
				 (setq data rest))))
		      (:digital ;; 0-1 signal (ex. valves)
		       NIL) ;; not implemented yet
		      (otherwise
		       (format t "wrong type : ~x~%" can-id)))
		    (format t "wrong payload size : ~x~%" can-id)))
	      (format t "unknown CAN frame: ~x~%" can-id)))
	(format t "empty xnet database~%"))))

(defparameter *stop* NIL)

(defun stop-reading-CAN-data ()
  (setq *stop* t))

(defun read-can-data (&key can-interface can-db-obj output-queues-analog)
  (setq *stop* NIL)
  (handler-case
      (with-can-socket
	  (sckt can-interface)
	  (let ((frame (make-can-packet)))
	    (with-poll (pc)
	      (poll-register pc (make-instance 'pollfd :fd sckt :events (poll-events :pollin)))
	      (loop until *stop* do
		(when (handler-case
			  (poll pc :timeout 100)
			(error (c)
			  (declare (ignore c))
			  NIL))
		  (socket-recvfrom sckt frame)
		  ;; with-mutex: in case someone wants to update CAN database on-the-fly
		  (sb-thread:with-mutex ((can-db-obj-lock can-db-obj))
		    (process-can-frame frame (can-db-obj-db can-db-obj) output-queues-analog)))))))
    (fsocket-error (c)
      (format t "Error: ~A~%" c)
      (sb-ext:exit))))


