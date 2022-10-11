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

(defun multicast (value queues)
  (mapc #'(lambda (queue) (sb-concurrency:enqueue value queue)) queues))
