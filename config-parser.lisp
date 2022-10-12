(in-package :can-logger.parser)

(define-condition cannot-parse-item (error)
  ((item :initarg :item :reader item))
  (:report (lambda (condition stream)
	     (format stream "Malformed item: \"~a\" ~&" (item condition)))))

(define-condition token-not-found (cannot-parse-item)   
  ((token :initarg :token :reader token))
  (:report (lambda (condition stream)
	     (format stream "Token \"~a\" not found in \"~a\" ~&" (token condition) (item condition)))))

(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)	  
       while line
       collect line)))

(defun split-list-of-strings-by-keyword (list keyword)
  "Returns fresh list of list segmenets separated by a keyword.
   How: look for a list item which contains the keyword and cons a difference 
   between the tail from that point and the original list. Then repeat recursively."
  (labels ((trav (lst keyword acc)
	     (if (null lst)
		 (nreverse acc)
		 (let ((next (member-if #'(lambda (item) (search keyword item)) (cdr lst))))
		   (trav next keyword (cons (ldiff lst next) acc))))))
    (trav list keyword NIL)))

(defun tokens-found-else-error (item list-of-tokens)
  "If some token in list-of-tokens is missing in item (string), an error is signaled."
  (mapc #'(lambda (token) (unless (search token item) (error 'token-not-found :item item :token token))) list-of-tokens))

(defun extract-words-between-tokens (string left-token right-token)
  "value = [1.1] -> 1.1 as string"
  (tokens-found-else-error string (list left-token right-token))
  (let ((start (search left-token string))
	 (end (search right-token string)))
    (subseq string (1+ start) end)))

;; todo: change to double precision
(defun parse-float (string)
  (with-input-from-string (in string)
    (read in)))

(defun trim-spaces (string)
  (string-trim '(#\Space #\Newline #\Backspace #\Tab 
		 #\Linefeed #\Page #\Return #\Rubout) string))
;; middle-level functions

(defun parse-integer-value (string keyword)
  "keyword = {X} -> X as integer"
  (tokens-found-else-error string (list keyword "="))
  (let ((value (parse-integer (extract-words-between-tokens string "{" "}") :radix 16 :junk-allowed t)))
    (unless value
      (error 'cannot-parse-item :item string))
    value))

(defun parse-string-value (string keyword)
  "keyword = {X} -> X as string"
  (tokens-found-else-error string (list keyword "="))
  (let ((value (trim-spaces (extract-words-between-tokens string "{" "}"))))
    value))

(defun parse-list-value (string keyword delimiterp)
  "Checks for keyword in string and splits it using delimiterp predicate."
  (tokens-found-else-error string (list keyword "="))
  (let ((values (extract-words-between-tokens string "{" "}")))
    (mapcar #'(lambda (item) (trim-spaces  item))
	    (split-sequence-by-delimiter values delimiterp))))

(defun parse-list-of-floats (string keyword)
  "Checks for keyword in string and converts elements in list to floats."
  (flet ((delimiterp (c) (char= c #\,)))
    (mapcar #'parse-float (parse-list-value string keyword #'delimiterp))))

(defun ignorep (string)
  "Checks whether a string is empty or is a comment"
  (or (string= string "") (char= (char string 0) #\#))) 

(defun try-parse (string parser)
  "Tries to parse a string with parser"
  (when (null parser)
    (error "No suitable parser found."))
  ;;(format t "trying..~a~%" parser)
  (restart-case (values parser (funcall parser string))
    (try-next-parser (next-parser)
      :report "trying next parser"
      (try-parse string next-parser))))

(defun parse-pair (pair)
  "(list 0 1 as str) -> (list 0 1 as int)"
  (let ((result))
    (push (parse-integer (first pair)) result)
    (when (second pair)
      (push (parse-integer (second pair)) result))
    (nreverse result)))

(defun compute-expected-payload-size (data-type-mask)
  "(:U8 :U16) -> 3"
  (flet ((keyword-to-byte (data-type)
	   (case data-type
	     (:bool 1)        ;; digital data (0/1) 
	     ((or :U8 :I8) 1) ;; analog data ... 
	     ((or :U16 :I16) 2)
	     ((or :U32 :I32) 4))))
    (reduce #'+ (mapcar #'keyword-to-byte data-type-mask))))

;; parsers

(defun parse-can-id (string)
  (parse-integer-value string "can_id"))

(defun parse-signal-type (string)
  (intern (string-upcase (parse-string-value string "signal")) :keyword))

(defun parse-endiannes (string)
  (let ((swap-p (parse-string-value string "byte_swap")))
    (if (string= swap-p "true")
	:le
	:be)))

(defun parse-data-type-mask (string)
  (flet ((delimiterp (c) (char= c #\,)))
    (mapcar #'(lambda (item) (intern (string-upcase item) :keyword)) (parse-list-value string "data_type_mask" #'delimiterp))))

(defun parse-bit-factor-mask (string)
  (parse-list-of-floats string "bit_factor_mask"))

(defun parse-physical-factor-mask (string)
  (parse-list-of-floats string "physical_factor_mask"))

(defun parse-physical-offset-mask (string)
  (parse-list-of-floats string "physical_offset_mask"))

(defun parse-label (string)
  (flet ((delimiterp (c) (char= c #\,)))
    (parse-list-value string "label" #'delimiterp)))

;; xnet-data definition

(defclass xnet-data ()
   ((can-id
     :accessor can-id
     :type (unsigned-byte 32))
    (signal-type
     :accessor signal-type
     :type symbol)
    (endiannes
     :accessor endiannes
     :type symbol)
    (data-type-mask
     :accessor data-type-mask
     :type cons)
    (bit-factor-mask
     :accessor bit-factor-mask
     :type cons)
    (physical-factor-mask
     :accessor physical-factor-mask
     :type cons)
    (physical-offset-mask
     :accessor physical-offset-mask
     :type cons)
    (payload-size
     :accessor payload-size
     :type (unsigned-byte 8))
    (label
     :accessor label
     :type cons)
    (multiplexed-p
     :accessor multiplexed-p
     :type atom)))

(defmethod print-object ((obj xnet-data) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((can-id can-id)			 
			 (signal-type signal-type)
			 (endiannes endiannes)
			 (data-type-mask data-type-mask)			 
			 (bit-factor-mask bit-factor-mask)
			 (physical-factor-mask physical-factor-mask)
			 (physical-offset-mask physical-offset-mask)
			 (label label)
			 (multiplexed-p multiplexed-p))			 
            obj
          (format stream "can-id: ~x~%signal-type: ~a~%endiannes: ~a~%data-type-mask: ~a~%bit-factor-mask: ~a~%physical-factor-mask: ~a~%physical-offset-mask: ~a~%label: ~a~%multiplexed-p: ~a~%"can-id signal-type endiannes data-type-mask bit-factor-mask physical-factor-mask physical-offset-mask label multiplexed-p))))


(defun map-item-to-xnet-data (item parsers tags)
  "Tries to find a suitable parser for each data string in item.
   If found, looks up a slot name associated with this parser
   and writes the parsed output to this slot. If no suitable
   parser found, raises an error."
  (let ((data (make-instance 'xnet-data)) 
	(dict (pairlis parsers tags))) ;; look-up table (parser : slot)
    (dolist (string item)
      (unless (ignorep string)
	(let ((list-of-parsers (copy-list parsers))) ;; copy because destructively modified
	  ;;(format t "parsing .. ~s ~%" string)
	  (multiple-value-bind (found-parser parsed-output)	      
	      (handler-bind ((cannot-parse-item (lambda (c)
						  (declare (ignore c))
						  (invoke-restart 'try-next-parser (pop list-of-parsers)))))
		(try-parse string (pop list-of-parsers)))	    	    
	    (let ((tag (cdr (assoc found-parser dict))))
	      ;;(format t "tag: ~a, string: ~a ~%" tag parsed-output)
	      ;;(format t "type: ~a~%" (type-of parsed-output))
	      (setf (slot-value data tag) parsed-output))))))
    (if (and
	 (eql :analog (slot-value data 'signal-type))
	 (> (length (slot-value data 'label)) (length (slot-value data 'data-type-mask))))
	(setf (slot-value data 'multiplexed-p) T)
	(setf (slot-value data 'multiplexed-p) NIL))
    (setf (slot-value data 'payload-size) (compute-expected-payload-size (slot-value data 'data-type-mask)))
    data))


(defparameter *list-of-parsers* (list #'parse-can-id
				      #'parse-signal-type
				      #'parse-endiannes
				      #'parse-data-type-mask				      
				      #'parse-bit-factor-mask
				      #'parse-physical-factor-mask
				      #'parse-physical-offset-mask
				      #'parse-label))

(defparameter *list-of-tags* (list 'can-id
				   'signal-type
				   'endiannes
				   'data-type-mask
				   'bit-factor-mask
				   'physical-factor-mask
				   'physical-offset-mask
				   'label))

(defun make-can-db (config-path) ;; todo cli
  (when (probe-file config-path)
    (let ((content (read-file config-path))
	  (can-db (make-hash-table))) 
      (dolist (item (split-list-of-strings-by-keyword content "can_id"))
	(let ((xnet-obj (map-item-to-xnet-data item *list-of-parsers* *list-of-tags*)))
	  (with-accessors ((can-id can-id)) xnet-obj
	    (setf (gethash can-id can-db) xnet-obj))))
      can-db)))
