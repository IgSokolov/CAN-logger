(in-package :can-logger.config-manager)

(defun add-new-entry (can-id config-path entry)
  (with-open-file (origin-stream config-path :direction :input)
    (with-open-file (target-stream "tmp-config" :direction :output :if-exists :supersede)
      (let ((can-id-string (format NIL "can_id={~a}" can-id))
	    (content (loop for line = (read-line origin-stream nil)
			   while line
			   collect (cons line (trim-spaces line t)))))	    
	(let ((start-idx (or
			 (position can-id-string content :key #'cdr :test #'string=)
			 (length content))))
	  (let ((end-idx (or
			  (position "" content :key #'cdr :test #'string= :start start-idx)
			  (length content))))
	    (format t "start-idx = ~a, end-idx = ~a~%" start-idx end-idx)
	    (loop for new-line in entry do
	      (write-line new-line target-stream))
	    (format target-stream "~%")
	    (loop for pair in content
		  for origin = (car pair)
		  for idx from 0 do
		    (when (or (< idx start-idx) (> idx end-idx))
		      (write-line origin target-stream)))))))
    (uiop:copy-file "tmp-config" config-path)
    (delete-file "tmp-config")))

;; todo: fix parameters. avoid assumptions.
(defun open-config-manager (can-id config-path)  
  (uiop:run-program (list "sh" "add-entry.sh" (format NIL "~x" can-id) config-path) :output t)
  (with-open-file (origin-stream "tmp_entry" :direction :input)
    (let ((entry (loop for line = (read-line origin-stream nil)
		       while line
		       collect line)))
      (add-new-entry can-id config-path entry)))
  (delete-file "tmp_entry"))
