(in-package :can-logger.tiles)

(defparameter *stop* NIL)

(defun close-widget-tiles ()
  (setq *stop* t))

(defstruct tile
  window
  gcontext
  gcontext-selected
  label
  font)

(defun draw-tile (tile)
  (let ((width (drawable-width (tile-window tile)))
	(height (drawable-height (tile-window tile)))
	(string-width (text-width (tile-gcontext tile) (format NIL "7FF")))
	(font-height (font-ascent (tile-font tile))))
    (let ((xc (round (/ (- width string-width) 2)))
	  (yc (round (/ (+ height font-height) 2))))      
      (draw-glyphs (tile-window tile) (tile-gcontext tile) xc yc (format NIL "~x" (tile-label tile))))))
        
(defun highlight (tile)
  (draw-rectangle (tile-window tile) (tile-gcontext-selected tile) 0 0 (drawable-width (tile-window tile)) (drawable-height (tile-window tile)) :fill-p)
  (sleep 0.1)
  (clear-area (tile-window tile))
  (draw-tile tile))

(defun create-tile (can-id window screen display colormap x0 y0 width height &optional (font "fixed"))
  (let* ((tile-window (create-window
		       :parent window
		       :x x0
		       :y y0
		       :width width
		       :height height
		       :border (screen-black-pixel screen)
		       :border-width 2
		       :bit-gravity :center
		       :event-mask '(:button-press)
		       :colormap colormap
		       :background (alloc-color colormap (lookup-color colormap "white"))))
	 (tile-gcontext (create-gcontext
			 :drawable tile-window
			 :font (open-font display font)
			 :line-style :solid
			 :background (screen-white-pixel screen)
			 :foreground (alloc-color colormap (lookup-color colormap "black"))))
	 (tile-gcontext-selected (create-gcontext
			 :drawable tile-window
			 :font (open-font display font)
			 :line-style :solid
			 :background (screen-white-pixel screen)
			 :foreground (alloc-color colormap (lookup-color colormap "gray")))))
    (map-window tile-window)
    (make-tile :window tile-window :gcontext tile-gcontext :gcontext-selected tile-gcontext-selected :label can-id :font (open-font display font))))  
  

(defun change-tile-position (tile-obj new-xc new-yc)
  (let ((old-xc (drawable-x (tile-window (cdr tile-obj))))
	(old-yc (drawable-y (tile-window (cdr tile-obj)))))
    (setf (drawable-x (tile-window (cdr tile-obj))) new-xc)
    (setf (drawable-y (tile-window (cdr tile-obj))) new-yc)    
    (values old-xc old-yc)))

(defun redraw-tile-window (dead-tile db)
  (let ((free-x0 (drawable-x (tile-window dead-tile)))
	(free-y0 (drawable-y (tile-window dead-tile))))
    (let ((new-xc free-x0)
	  (new-yc free-y0))
      (loop for tile-obj in db
	    for window = (tile-window (cdr tile-obj))	    
	    for xc = (drawable-x window)
	    for yc = (drawable-y window) do
	      (multiple-value-bind (old-xc old-yc) (change-tile-position tile-obj new-xc new-yc)
		(setq new-xc old-xc
		      new-yc old-yc)))
      db)))

(defun make-widget-tiles (&key can-db-obj main-window display screen colormap data-queue config-path)
  (setq *stop* NIL)
  ;; db serves as a database for the tiles already present on the widget
  ;; can-db-obj is a global database with xnet entries.
  (let ((db) ;; todo: comment diff db vs can-db
	(db-lock (sb-thread:make-mutex))
	(window (create-window
		      :parent main-window
		      :x 5
		      :y 5
		      :width (- (drawable-width main-window) 15)
		      :height (- (drawable-height main-window) 15)
		      :border (screen-black-pixel screen)
		      :border-width 2
		      :bit-gravity :center
		      ;;:event-mask '(:button-press :button-release)
		      :colormap colormap
		      :background (alloc-color colormap (lookup-color colormap "white")))))
    (let ((tile-width 80)
	  (tile-height 40)
	  (max-x (- (drawable-width window) 15))
	  (xc 0)
	  (yc 0)
	  (most-recent-can-id 0))
      (map-window main-window)
      (map-window window)
      ;; when a user clicks on a tile, the config window opens
      ;; and the use can create a new entry
      (sb-thread:make-thread
	   (lambda ()
	     (with-safe-exit-on-window-closed
	       (loop until *stop* do
		   (event-case (display :force-output-p t :timeout 0.1)
		     (:button-press (window)
				    (sb-thread:with-mutex (db-lock)
				      (sb-thread:with-mutex ((can-db-obj-lock can-db-obj))
					(let* ((tile (find window db :key #'(lambda (x) (tile-window (cdr x))) :test 'equal))
					       (can-id (car tile)))					  
					  (setf db (remove tile db))
					  (open-config-manager can-id config-path)
					  (setf db (redraw-tile-window (cdr tile) db))
					  ;; shift tiles backwards
					  (if (= xc 0) ;; todo: fix compiler warnings
					      (progn
						(setq xc (- max-x tile-width))
						(decf yc (+ 5 tile-height)))					      
					      (decf xc (+ tile-width 5)))
					  (destroy-window (tile-window (cdr tile)))					  
					  (setf (can-db-obj-db can-db-obj) (make-can-db config-path)))))
					;; todo: error handling, because user gives wrong input.
				    t) 
		     (otherwise () t))
		     (sleep 0.01)))))
      
      (loop until *stop* do	
	(let ((can-id (sb-concurrency:dequeue data-queue)))
	  ;; doesn't acquire mutex, but checks if its free.
	  ;; we want to keep the loop alive and keep reading
	  ;; *stop* value
	  (if (and can-id (not (sb-thread:mutex-value db-lock)))
	      (let ((tile (cdar (member can-id db :key #'car))))
		(if tile
		    (unless (= can-id most-recent-can-id)
		      (setq most-recent-can-id can-id)
		      (highlight tile))
		    (let ((new-tile (create-tile can-id window screen display colormap xc yc tile-width tile-height)))
		      ;; shift tiles forwards
		      (incf xc (+ tile-width 5))
		      (when (< (- max-x xc tile-width) tile-width)			
			(setq xc 0)
			(incf yc (+ 5 tile-height)))
		      (push (cons can-id new-tile) db)
		      (draw-tile new-tile)))
		(display-force-output display))
	      (sleep 0.1)))))))



