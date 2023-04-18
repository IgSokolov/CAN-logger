(in-package :can-logger.cli)

(define-condition parse-error-report (error)
  ((msg :initarg :msg :reader msg))
  (:report (lambda (condition stream)
	     (format stream "Error: ~S ~&" (msg condition)))))

(defun ensure-file-exists (path)
  "Checks whether path is correct"
  (unless (uiop:file-exists-p path)
    ;; This is a temporary limitation.
    ;; The app has functionality to create
    ;; and load a missing/broken config-file on-the-fly,
    ;; but the widget for that is not ready yet.
    ;; Therefore we exit now.
    (error 'parse-error-report :msg "Config path doesn't exist"))
  path)

(defun check-can-interface (interface-name)  
  (let ((len (length interface-name)))
    (let ((interface-number (parse-integer (string (elt interface-name (- len 1))) :junk-allowed t))
	  (name-body (subseq interface-name 0 (- len 1))))
      (unless interface-number	 
	(error 'parse-error-report
	       :msg (format nil "Name of CAN interface must have be a positive integer at the end (ex. vcan0)")))
      (unless (search "can" name-body)
	(error 'parse-error-report
	       :msg (format nil "Name of CAN interface must include a `can` substring (ex. vcan0)")))))
  interface-name)

(defun check-time (string-value &key mode)
  (handler-case
      (let ((value (parse-integer string-value))
	    (t-unit (case mode
		       (:dt "milliseconds")
		       (:t-max "seconds"))))
	(check-type value integer)	
	(unless (plusp value)
	  (error 'parse-error-report
		 :msg (format nil "Time in ~a must be a positive integer" t-unit)))
	value)
    (SB-INT:SIMPLE-PARSE-ERROR (c)
      (declare (ignore c))
      (error 'parse-error-report :msg (format nil "Cannot convert ~a to integer" string-value)))))

;; add plot settings
(defun parse-cli-args ()  
  (opts:define-opts
    (:name :help
	   :description "prints help"
	   :short #\h
	   :long "help")
    (:name :path
	   :description "path to config file"
	   :short #\p
	   :arg-parser #'identity ;; bypass conditions signaling used in library
	   :long "path")	   
    (:name :can-interface
	   :description "name of CAN interface"
	   :short #\c
	   :arg-parser #'identity
     :long "can-interface")
    (:name :dt
	   :description "sampling time in milliseconds"
	   :short #\s
	   :arg-parser #'identity
     :long "sampling-time")
    (:name :t-max
	   :description "width of time window in seconds"
	   :short #\w
	   :arg-parser #'identity
     :long "window-width"))
	   
  (multiple-value-bind (options free-args)
      (handler-case (opts:get-opts)
	(error (c)
	  (format t "~a ~&" c)
	  (sb-ext:exit)))
    (declare (ignore free-args))
    
    (when (getf options :help)        
      (opts:describe
       :prefix "CAN bus monitor\
Usage example: ./can-logger -p ./config -c vcan0 -s 100 -w 60")
      (sb-ext:exit))
          
    ;; check required args
    (unless (getf options :path)
      (format t "Error: --path is not provided~&")
      (sb-ext:exit))

    (unless (getf options :can-interface)
      (format t "Error: --can-interface is not provided~&")
      (sb-ext:exit))

    (unless (getf options :dt)
      (format t "Error: --sampling-time is not provided~&")
      (sb-ext:exit))
    (unless (getf options :t-max)
      (format t "Error: --window-width is not provided~&")
      (sb-ext:exit))
      
    ;; set main parameters
    (handler-case (values 
		   (ensure-file-exists (getf options :path))
		   (check-can-interface (getf options :can-interface))
		   (check-time (getf options :dt) :mode :dt)
		   (check-time (getf options :t-max) :mode :t-max))
      (parse-error-report (c)
	(format t "~a" c)      
	(sb-ext:exit)))))
