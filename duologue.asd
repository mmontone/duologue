(asdf:defsystem #:duologue
  :description "High-level user interaction library for Common Lisp"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "duologue"))
  :depends-on (:anaphora
	       :alexandria
	       :clavier 
	       :chronicity
	       :cl-readline
	       :cl-ansi-text
	       :drakma
	       :cl-fad))
