(in-package can-logger.utils)

(defmacro with-safe-exit-on-window-closed (&body body)
  `(handler-case
       (progn ,@body)
     (SB-INT:BROKEN-PIPE (c)       
       (declare (ignore c))
       NIL)
     (END-OF-FILE (c)
       (declare (ignore c))
       NIL)))
