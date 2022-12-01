(defpackage :duologue/tests
  (:use :cl :stefil :duologue)
  (:export :run-tests))

(in-package :duologue/tests)

(defsuite* duologue-tests)

(defun run-tests ()
  (duologue-tests))

(deftest prompt-test ()
  (with-input-from-string (*query-io* "something
")
    (is (string= (prompt "Something:") "something")))

  (signals error
    (with-input-from-string (*query-io* "something")
      (prompt "Integer:" :parser #'parse-integer)))

  (with-input-from-string (*query-io* "22")
    (is (= (prompt "Integer:" :parser #'parse-integer) 22)))

  (with-input-from-string (*query-io* "aa
22")
    (is (= (prompt "Integer:" :parser #'parse-integer) 22)))
  
  (signals error (with-input-from-string (*query-io* "lala
")
    (prompt-integer "Integer: "))))
