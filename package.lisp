(defpackage #:duologue
  (:use #:cl #:anaphora)
  (:export :ask
	   :msg
	   :msg*
	   :choose
	   :choose-many
	   :prompt
	   :prompt-integer
	   :prompt-email
	   :prompt-url
	   :prompt-datetime
	   :parse-prompt)
  (:documentation "High-level interaction library for Common Lisp"))
