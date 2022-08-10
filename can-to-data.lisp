(in-package :can-logger.can2data)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency))
;; (defstruct can-packet
;;   id
;;   data
;;   timestamp
;;   origin)

;; (defun parse-can-packet (packet)
;;   (values
;;    (can-packet-id packet)
;;    (can-packet-data packet)
;;    (can-packet-timestamp packet)
;;    (can-packet-origin packet)))

(defstruct plot-data y label)

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

(defun make-plot-timestamp (can-timestamp)
  (destructuring-bind (t-sec t-usec) can-timestamp
    (float (+ t-sec (/ t-usec 1000000))))) ;; full-time
    	
(defparameter *plot-queue* (sb-concurrency:make-queue :name 'plot-queue :initial-contents NIL))
(defun process-can-frame (can-frame)
  "decode data and send it to plotter"
  (multiple-value-bind (can-id data timestamp origin) (parse-can-packet can-frame)
    (declare (ignore timestamp))
    (format t "(do we need it ?) origin = ~a~%" origin)
    (let ((xnet-item (gethash can-id *can-db*)))
      (when xnet-item
	(with-accessors ((signal-type signal-type)
			 (endiannes endiannes)
			 (data-type-mask data-type-mask)			 
			 (bit-factor-mask bit-factor-mask)
			 (physical-factor-mask physical-factor-mask)
			 (physical-offset-mask physical-offset-mask)
			 (label label)
			 (multiplexed-p multiplexed-p)) xnet-item
	  ;;(print xnet-item)
	  (loop for data-type in data-type-mask		
		for bit-factor in bit-factor-mask
		for physical-factor in physical-factor-mask
		for physical-offset in physical-offset-mask
		for l in label do
		  (multiple-value-bind (bytes rest) (pick-bytes data data-type)		   
		    (let ((plot-value (make-plot-data
				       :y (+ physical-offset
					     (* bit-factor
						physical-factor
						(bytes-to-integer bytes data-type endiannes)))				       
				       :label l)))
		      (sb-concurrency:enqueue plot-value *plot-queue*))		      
		    (setq data rest)))))))) ;; todo multiplexed


(defun generate-data (n)
  (let ((switch t))
    (loop for i from 0 upto n do
      (let ((plot-value-1 (make-plot-data
			   :y (* 0.5 (sin (* 2 pi 0.1 i)))
			   ;;:y (random 10)
			   ;;:y 0.5
			   :label "label-1"))
	    (plot-value-2 (make-plot-data
			   :y (* 0.1 (sin (* 2 pi 0.1 i)))
			   ;;:y (random 10)
			   ;;:y 0.5
			   :label "label-2")))
	(when (= 0 (mod i 10))
	  (setq switch (not switch)))
	(if switch
	    ;;(sb-concurrency:enqueue NIL *plot-queue*)
	    (sb-concurrency:enqueue plot-value-1 *plot-queue*)	 
	    (sb-concurrency:enqueue plot-value-2 *plot-queue*)
	    )))))


(defun read-can-data ()
  (with-can-socket (sckt "vcan0")
		   (let ((frame (make-can-packet)))
		     (loop :repeat 3 do
		       (socket-recv sckt frame)		    
		       (process-can-frame frame)))))

