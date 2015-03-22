(defpackage #:duologue
  (:use #:cl #:anaphora)
  (:export :say
	   :ask
	   :choose
	   :choose-many
	   :prompt
	   :prompt-integer
	   :prompt-email
	   :prompt-url
	   :prompt-datetime
	   :prompt-pathname
	   :parse-prompt)
  (:documentation "High-level interaction library for Common Lisp"))
