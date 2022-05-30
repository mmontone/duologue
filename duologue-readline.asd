(asdf:defsystem #:duologue-readline
  :description "High-level user interaction library for Common Lisp"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license "GPL v3"
  :homepage "https://github.com/mmontone/duologue"		
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))		
  :serial t
  :components ((:file "package")
	       (:file "readline")
               (:file "duologue"))
  :depends-on (:anaphora
	       :alexandria
	       :clavier 
	       :chronicity
	       :cl-readline
	       :cl-ansi-text
	       :drakma
	       :cl-fad))
