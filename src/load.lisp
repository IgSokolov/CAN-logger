(require "asdf")
(push (uiop/os:getcwd) asdf:*central-registry*)
(asdf:make "can-logger")


