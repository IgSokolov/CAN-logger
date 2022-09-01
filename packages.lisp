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
   :label
   :*can-db*))

(defpackage :can-logger.can2data
  (:use :cl :can-logger.parser :fsocket)
  (:export
   
   :plot-data-value
   :plot-data-label
   :plot-data-can-id
   :make-plot-data
   :*plot-queue*))

(defpackage :can-logger.table
  (:use :cl :xlib :can-logger.can2data)
  (:export :make-widget-table :close-widget-table))
  

(defpackage :can-logger.plotter
  (:use :cl :can-logger.can2data :xlib)
  (:import-from :can-logger.parser :split-sequence-by-delimiter)
  (:export :make-widget-plot :close-widget-plot))

(defpackage :can-logger.main
  (:use :cl :xlib :can-logger.table :can-logger.plotter :can-logger.can2data))
  
