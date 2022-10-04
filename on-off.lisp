(in-package :can-logger.on-off)

(defparameter *stop* NIL)
(defun close-widget-on-off ()
  (setq *stop* t))

(defun make-widget-on-off (&key main-window display screen colormap data-queue)
  (sb-thread:make-thread
   (lambda ()
     (loop for data = (sb-concurrency:dequeue data-queue) until *stop* do
       (format t "el = ~a~%" data)
       (sleep 0.5)))))
