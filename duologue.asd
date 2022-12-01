(asdf:defsystem #:duologue
  :description "High-level user interaction library for Common Lisp"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mmontone/duologue"		
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))		
  :serial t
  :components ((:file "package")
	       (:file "nocompletion")
               (:file "duologue"))
  :depends-on (:anaphora
	       :alexandria
	       :clavier 
	       :chronicity
	       :cl-ansi-text
	       :cl-fad)
  :in-order-to ((asdf:test-op (asdf:test-op :duologue-test))))
