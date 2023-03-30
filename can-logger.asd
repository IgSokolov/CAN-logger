(asdf:defsystem "can-logger"
  :description ""
  :author "Dr.-Ing. Igor Sokolov"
  :licence "BSD"
  :version "1.0.0"
  :build-operation program-op
  :build-pathname "./can-logger"
  :entry-point "can-logger.main:run"  
  :components ((:module "src"
			:components	
			((:file "packages")
			 (:file "config-parser")
			 (:file "can-to-data")
			 (:file "colors")
			 (:file "utils")	       
			 (:file "plotter")
			 (:file "cli")
			 (:file "main"))))  
  :depends-on (:fsocket :clx :unix-opts))
	       
