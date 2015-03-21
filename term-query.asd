(asdf:defsystem #:term-query
  :description "High-level terminal interaction library"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "term-query"))
  :depends-on (:anaphora
	       :alexandria
	       :clavier 
	       :chronicity
	       :cl-readline
	       :cl-ansi-text))
