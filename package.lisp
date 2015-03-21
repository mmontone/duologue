(defpackage #:term-query
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
	   :parse-prompt))
