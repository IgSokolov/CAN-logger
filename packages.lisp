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

   :plot-data-timestamp
   :plot-data-y
   :*plot-queue*))

(defpackage :can-logger.plotter
  (:use :cl :can-logger.can2data :xlib)
  (:import-from :can-logger.parser :split-sequence-by-delimiter))
  
