(asdf:defsystem "can-logger"
  :description ""
  :author "Dr.-Ing. Igor Sokolov"
  :licence "BSD"
  :version "1.0.0"
  :components ((:file "packages")
	       (:file "config-parser")
	       (:file "can-to-data")
	       (:file "utils")
	       (:file "clx-table")
	       (:file "clx-button")
	       (:file "plotter")
	       (:file "clx-tiles")
	       (:file "main"))	       
  :depends-on (:fsocket :clx))
	       
