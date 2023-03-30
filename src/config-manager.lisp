(in-package :can-logger.config-manager)

(defun add-new-entry (can-id config-path)
  (with-open-file (origin-stream config-path :direction :input)    
    (with-open-file (target-stream "tmp-config" :direction :output :if-exists :supersede)
      (let ((can-id-string (format NIL "can_id={~x}" can-id))
	    (content (loop for line = (read-line origin-stream nil)
			   while line
			   collect (cons line (trim-spaces line t))))
	    entry)	    
	(let* ((content-length (length content))
	       (start-idx-default content-length)
	       (end-idx-default content-length)) ;; any value here
	  (let ((start-idx (position can-id-string content :key #'cdr :test #'string=)))
	    (if start-idx
		;; entry exists
		(let ((end-idx (or
				(position "" content :key #'cdr :test #'string= :start start-idx)
				content-length)))
		  (setq start-idx-default start-idx
			end-idx-default end-idx)
		  ;; fetch old entry from the content and write it to file
		  (with-open-file (tmp-stream "tmp-entry" :direction :output :if-exists :supersede)
		    (loop for line in (subseq (mapcar #'car content) start-idx end-idx) do
		      (write-line line tmp-stream)))
		  ;; modify the file with gedit
		  (uiop:run-program (list "sh" "open-file-with-gedit.sh" "tmp-entry") :output t)
		  ;; read new entry back from the file
		  (with-open-file (tmp-stream "tmp-entry" :direction :input)
		    (setq entry (loop for line = (read-line tmp-stream nil)
				      while line
				      collect line))))
		(progn ;; entry doesn't exist
		  (uiop:run-program (list "sh" "add-entry-with-gedit.sh" (format NIL "~x" can-id) "tmp-entry") :output t)
		  (setq entry (with-open-file (origin-stream "tmp-entry" :direction :input)
				 (loop for line = (read-line origin-stream nil)
				       while line
				       collect line))))))	  
	  (delete-file "tmp-entry")
	  ;; always write modifications to the head of file
	  (loop for new-line in entry do
	    (write-line new-line target-stream))
	  (format target-stream "~%")
	  ;; then write the old (unmodified) content to file
	  (loop for pair in content
		for origin = (car pair)
		for idx from 0 do
		  (when (or (< idx start-idx-default) (> idx end-idx-default))
		    (write-line origin target-stream)))))))
  (uiop:copy-file "tmp-config" config-path)
  (delete-file "tmp-config"))
 




