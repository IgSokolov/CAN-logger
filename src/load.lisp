(require "asdf")
(push (uiop/os:getcwd) asdf:*central-registry*)
;;(asdf:load-system "can-logger")
(asdf:make "can-logger")


