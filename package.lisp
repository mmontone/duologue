(defpackage #:term-query
  (:use #:cl)
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
