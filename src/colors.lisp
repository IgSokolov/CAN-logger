(in-package :can-logger.colors)

(defstruct rgb-color r g b)
(defstruct color-ring-buffer
  ref-colors
  available-colors
  n-of-available-colors
  n-of-colors
  (n-colors-used 0))

(defun init-color-ring-buffer ()
  (let ((rc (list
	     (make-rgb-color :r 0 :g 183 :b 235)
	     (make-rgb-color :r 255 :g 69 :b 0)
	     (make-rgb-color :r 255 :g 140 :b 0)
	     (make-rgb-color :r 57 :g 255 :b 20)
	     (make-rgb-color :r 32 :g 178 :b 170)
	     (make-rgb-color :r 254 :g 78 :b 218))))
    (make-color-ring-buffer :ref-colors rc
			    :n-of-colors (list-length rc)
			    :available-colors (copy-list rc)
			    :n-of-available-colors (list-length rc))))

(defparameter *color-ring* (init-color-ring-buffer))

(defun reset-color-ring-buffer (color-buffer)
  (setf (color-ring-buffer-n-of-available-colors color-buffer)
	(color-ring-buffer-n-of-colors color-buffer))
  (setf (color-ring-buffer-available-colors color-buffer)
	(copy-list (color-ring-buffer-ref-colors color-buffer))))
  
(defun pick-up-color ()
  (when (= (color-ring-buffer-n-of-available-colors *color-ring*) 0)
    (reset-color-ring-buffer *color-ring*))
  (let ((col (pop (color-ring-buffer-available-colors *color-ring*))))
    (decf (color-ring-buffer-n-of-available-colors *color-ring*))
    (make-color :red (/ (rgb-color-r col) 255.0)
		:blue (/ (rgb-color-b col) 255.0)
		:green (/ (rgb-color-g col) 255.0))))


