(in-package can-logger.utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency))

(defmacro with-safe-exit-on-window-closed (&body body)
  `(handler-case
       (progn ,@body)
     (SB-INT:BROKEN-PIPE (c)       
       (declare (ignore c))
       NIL)
     (END-OF-FILE (c)
       (declare (ignore c))
       NIL)))

;; https://stackoverflow.com/questions/15393797/lisp-splitting-input-into-separate-strings
(defun split-sequence-by-delimiter (sequence delimiterp)
  "Lisp-is-cool-> (Lisp is cool) as strings. Delimiterp is a predicate."
  (loop :for beg = (position-if-not delimiterp sequence)
     :then (position-if-not delimiterp sequence :start (1+ end))
     :for end = (and beg (position-if delimiterp sequence :start beg))
     :when beg :collect (subseq sequence beg end)
     :while end))

(defun multicast (value queues)
  (mapc #'(lambda (queue) (sb-concurrency:enqueue value queue)) queues))

(defstruct can-db-obj
  db
  (lock (sb-thread:make-mutex)))
