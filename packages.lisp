(defpackage :can-logger.parser
  (:use :cl)
  (:export
   :split-sequence-by-delimiter
   :can-id
   :signal-type
   :endiannes
   :data-type-mask
   :byte-number-mask
   :bit-factor-mask
   :physical-factor-mask
   :physical-offset-mask
   :payload-size
   :label
   :*can-db*))

(defpackage :can-logger.can2data
  (:use :cl :can-logger.parser :fsocket)
  (:export
   :read-can-data
   :stop-reading-CAN-data
   :plot-data-value
   :plot-data-label
   :plot-data-can-id
   :make-plot-data))

(defpackage :can-logger.utils
  (:use :cl)
  (:export :with-safe-exit-on-window-closed :multicast))
	   
(defpackage :can-logger.table
  (:use :cl :xlib :can-logger.can2data :can-logger.utils)
  (:export :make-widget-table :close-widget-table))

(defpackage :can-logger.button
  (:use :cl :xlib :can-logger.utils)
  (:export :make-widget-button))

(defpackage :can-logger.plotter
  (:use :cl :can-logger.can2data :xlib :can-logger.utils)
  (:import-from :can-logger.parser :split-sequence-by-delimiter)
  (:export :make-widget-plot :close-widget-plot))

(defpackage :can-logger.config-manager
  (:use :cl :can-logger.can2data :xlib :can-logger.utils)
  (:export :open-config-manager))

(defpackage :can-logger.tiles
  (:use :cl :can-logger.can2data :xlib :can-logger.utils :can-logger.config-manager)
  (:export :make-widget-tiles :close-widget-tiles))

(defpackage :can-logger.on-off
  (:use :cl :can-logger.can2data :xlib :can-logger.utils)
  (:export :make-widget-on-off :close-widget-on-off))

(defpackage :can-logger.main
  (:use :cl :xlib :can-logger.table :can-logger.plotter :can-logger.can2data :can-logger.button :can-logger.tiles :can-logger.on-off))
  
