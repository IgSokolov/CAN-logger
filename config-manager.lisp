(in-package :can-logger.config-manager)

(defun open-config-manager (can-id config-path)
   (uiop:run-program (list "sh" "add-new-entry.sh" (format NIL "~x" can-id) config-path) :output t))
