(asdf:defsystem #:term-query
  :description "Describe term-query here"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "term-query"))
  :depends-on (:anaphora :clavier :chronicity))

