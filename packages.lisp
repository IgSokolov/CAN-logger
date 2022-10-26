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

(defpackage :can-logger.can2data
  (:use :cl :can-logger.parser :fsocket :can-logger.utils)
  (:export
   :read-can-data
   :stop-reading-CAN-data
   :plot-data-value
   :plot-data-label
   :plot-data-can-id
   :make-plot-data))
	   
(defpackage :can-logger.table
  (:use :cl :xlib :can-logger.can2data :can-logger.utils)
  (:export :make-widget-table :close-widget-table))

(defpackage :can-logger.button
  (:use :cl :xlib :can-logger.utils)
  (:export :make-widget-button))

(defpackage :can-logger.plotter
  (:use :cl :can-logger.can2data :xlib :can-logger.utils)
  (:export :make-widget-plot :close-widget-plot))

(defpackage :can-logger.config-manager
  (:use :cl :can-logger.can2data :xlib :can-logger.utils)
  (:export :open-config-manager))

(defpackage :can-logger.tiles
  (:use :cl :can-logger.can2data :xlib :can-logger.utils :can-logger.config-manager :can-logger.parser)
  (:export :make-widget-tiles :close-widget-tiles))

(defpackage :can-logger.on-off
  (:use :cl :can-logger.can2data :xlib :can-logger.utils)
  (:export :make-widget-on-off :close-widget-on-off))

(defpackage :can-logger.main
  (:use :cl :xlib :can-logger.utils :can-logger.table :can-logger.parser :can-logger.plotter :can-logger.can2data :can-logger.button :can-logger.tiles :can-logger.on-off))
  
