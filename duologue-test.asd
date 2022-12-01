(asdf:defsystem #:duologue-test
  :license "MIT"
  :description "Tests for Duologue"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :depends-on (#:duologue #:stefil)
  :components
  ((:file "tests"))
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :duologue/tests :run-tests)))
