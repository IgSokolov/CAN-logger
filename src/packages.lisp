(defpackage :can-logger.utils
  (:use :cl)
  (:export
   :with-safe-exit-on-window-closed
   :multicast
   :split-sequence-by-delimiter
   
   :make-can-db-obj
   :can-db-obj-db
   :can-db-obj-lock
   :trim-spaces))

(defpackage :can-logger.parser
  (:use :cl :can-logger.utils)
  (:export
   :endiannes
   :make-can-db
   :payload-size
   :can-id
   :signal-type
   :data-type-mask
   :bit-factor-mask
   :physical-factor-mask
   :physical-offset-mask
   :label
   :multiplexed-p))

(defpackage :can-logger.colors
  (:use :cl :xlib)
  (:export
   :pick-up-color))

(defpackage :can-logger.can2data
  (:use :cl :can-logger.parser :fsocket :can-logger.utils)
  (:export
   :read-can-data
   :stop-reading-CAN-data
   :plot-data-value
   :plot-data-label
   :plot-data-can-id
   :make-plot-data))
	   
(defpackage :can-logger.plotter
  (:use :cl :can-logger.can2data :xlib :can-logger.utils :can-logger.colors)
  (:export :make-widget-plot :close-widget-plot))

(defpackage :can-logger.cli
  (:use :cl)
  (:export :parse-cli-args))

(defpackage :can-logger.main
  (:use :cl :xlib :fsocket :can-logger.utils :can-logger.parser :can-logger.plotter :can-logger.can2data :can-logger.cli)
  (:export :run))
  
