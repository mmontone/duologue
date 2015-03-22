(in-package #:duologue)

(defparameter *prompt-color* nil "The default prompt color.")
(defparameter *prompt-error-color* nil "The default error color")

(defun remove-options (args &rest keys)
  (let ((args (copy-list args))
	(new-args nil))
    (loop 
	 with arg
	 while args
	 do (setf arg (pop args))
	 (if (member arg keys)
	     (pop args)
	     (push arg new-args)))
    new-args))

(defun find-option (args option)
  (let ((args (copy-list args)))
    (loop with arg
       while args
       do (setf arg (pop args))
	 (when (equalp arg option)
	   (return-from find-option (car args))))
    nil))

(defun make-completer (options)
  (lambda (text start end)
    (declare (ignorable start end))
    (labels ((common-prefix (items)
             (subseq
              (car items) 0
              (position
               nil
               (mapcar
                (lambda (i)
                  (every (lambda (x)
                           (char= (char (car items) i)
                                  (char x           i)))
                         (cdr items)))
                (alexandria:iota (reduce #'min (mapcar #'length items)))))))
           (select-completions (list)
             (let ((els (remove-if-not (alexandria:curry #'alexandria:starts-with-subseq text)
                                       list)))
               (if (cdr els)
                   (cons (common-prefix els) els)
                   els))))
      (select-completions options))))
  
(defun say (datum &rest args)
  "Prints a message on the screen.

   Args: - datum(string): A format like string.
         - args: Format arguments or :color, :newline options
         - color(keyword): An ansi-text color. One of ansi-colors (.i.e :red, :green, :yellow)
         - newline(boolean): If t, forces a newline after printing

   A newline is printed iff either newline parameter is T or datum doesn't end with a space. That is, if datum ends in a space, then no newline is printed.

   Example:
 
   ``(say \"Hello ~A\" \"John\" :color :blue)``
   
   Categories: printing
   Tags: printing"  
  (let ((format-args (remove-options args :color :newline))
	(color (find-option args :color))
	(newline (find-option args :newline)))
    (if color
	 (cl-ansi-text:with-color (color)
	   (apply #'format t (cons datum format-args)))
	 ; else
	 (apply #'format t (cons datum format-args)))
    (when (or newline
	      (not (cl-ppcre:scan "[ \\t](\\e\\[\\d+(;\\d+)*m)?\\Z" datum)))
      (terpri))))
  
(defun choose (msg options &key if-wrong-option 
			     default
			     (print-options t)
			     (separator "~%")
			     complete
			     (color *prompt-color*)
			     (error-color *prompt-error-color*))
  "Asks the user to choose one of the given options.

   Args: - msg(string): The prompt message.
         - options(list): The list of options the user can choose from.
         - if-wrong-option(function): When present, this function is run if the user enters a wrong option. Default: nil.
         - default: The default value. The default value is selected if the user just hits the ENTER key. Default: nil.
         - print-options(boolean): Print the options on the screen. Default: T.
         - separator(string): Separation string to use when printing the options. Default: '~%'
         - complete: If T, then readline completion is enabled. Default: nil.
         - color: Color to use at prompt. Default: *prompt-color*
         - error-color: Color to use when error ocurrs. Default: *prompt-error-color*

  Example: 

  ``(choose \"Choose: \" (list \"foo\" \"bar\" \"baz\") :default \"baz\")``

  Tags: menu, choose" 
  (flet ((print-options ()
	   (loop 
	      for option in options
	      for i from 0 
	      do
		(format t "[~A] ~A" i option)
		(when (< (1+ i) (length options))
		  (format t separator)))
	   (terpri)
	   (say msg :color color)
	   (when default
	     (say "[~A] " default :color color)))
	 (read-option ()
	   (if complete
	       (progn
		 (rl:register-function :complete (make-completer options))
		 (rl:readline :prompt (format nil "~A~@[[~A]~]" msg default)))
	       (read-line))))
    (when print-options
      (print-options))
    (let* ((chosen-option (read-option))
	   (option-number (ignore-errors (parse-integer chosen-option))))
      (loop 
	 do 
	   (cond ((and (equalp chosen-option "")
		       default)
		  (return default))
		 ((find chosen-option (mapcar #'princ-to-string options) :test #'string=)
		  (return (find chosen-option (mapcar #'princ-to-string options) :test #'string=)))
		 ((and option-number
		       (>= option-number 0)
		       (< option-number (length options)))
		  ;; Correct option
		  (return (nth option-number options)))
		 (t
		  ;; Incorrect option
		  (progn
		    (if if-wrong-option
			(funcall if-wrong-option)
			(say "Wrong option." :color error-color))
		    (when print-options
		      (print-options)))))
	   (setf chosen-option (read-option))
	   (setf option-number (ignore-errors (parse-integer chosen-option)))))))
  
(defun ask (&optional (msg "Yes or no: ") &key 
					    (default nil default-p) 
					    if-wrong-answer
					    (color *prompt-color*)
					    (error-color *prompt-error-color*))
  "Ask for yes or no.

   Args: - msg(string): The prompt to use. Default: 'Yes or no: '.
         - default: Default value. It gets selected if the user enters the empty string. Default: nil.
         - if-wrong-answer(function): Function to execute if a wrong answer is given.
         - color: Prompt color.
         - error-color: Prompt error color."
  (check-type default boolean)
  (labels ((format-boolean (boolean)
	     (if boolean "yes" "no"))
	   (ask-question ()
	     (say msg :color color)
	     (when default-p
	       (say "[~A] " (format-boolean default) :color color))))
    (ask-question)
    (let ((answer (read-line)))
      (loop
	   do
	   (cond 
	     ((and (equalp answer "") default-p)
	      (return-from ask default))
	     ((member answer (list "yes" "y" "on" "true") :test #'string-equal)
	      (return-from ask t))
	     ((member answer (list "no" "n" "off" "false") :test #'string-equal)
	      (return-from ask nil))
	     (t 
	      (if if-wrong-answer
		  (funcall if-wrong-answer)
		  (say "Answer yes or no" :color error-color))
	      (ask-question)
	      (setf answer (read-line))))))))

(defun prompt (&optional msg &key default 
			       (required-p t) 
			       validator 
			       if-invalid
			       (color *prompt-color*)
			       (error-color *prompt-error-color*))
  "Prompt for a string.

   Args: - msg: The prompt.
         - default: Default value. This is returned if the user enters the empty string. Default: nil.
         - required-p(boolean): If T, then the empty string is not allowed as a valid input, and the user is asked again for input. Default: t.
         - validator(function): A function to use to validate the input. Should return T if the input is valid, or NIL otherwise.
         - if-invalid(function): Function to execute if the validator fails.
         - color: Prompt color
         - error-color: Prompt error color."         
  (loop do
       (when msg
	 (say msg :color color))
       (when default
	 (say "[~A] " default :color color))
       (let ((input (read-line)))
	 (cond ((and (string-equal input "") default)
		(return default))
	       ((and (string-equal input "") required-p)
		(say "A non empty value is required" :color error-color))
	       ((and validator
		     (not (funcall validator input)))
		(if if-invalid
		    (funcall if-invalid input)
		    (say "The value is not valid" :color error-color)))
	       (t
		(return input))))))

(defun parse-prompt (parser &optional msg &key default 
					    (required-p t) 
					    validator
					    if-invalid
					    (color *prompt-color*)
					    (error-color *prompt-error-color*))
  "Like prompt, but parses its input.

   Args: - parser: A function that parses the input string. Should return NIL if it cannot parse.
         - msg: The prompt.
         - default: Default value. This is returned if the user enters the empty string. Default: nil.
         - required-p(boolean): If T, then the empty string is not allowed as a valid input, and the user is asked again for input. Default: t.
         - validator(function): A function to use to validate the input. Should return T if the input is valid, or NIL otherwise.
         - if-invalid(function): Function to execute if the validator fails.
         - color: Prompt color
         - error-color: Prompt error color.

  Returns: The parsed value or the default value, depending on what the user entered"
  (loop do
       (when msg
	 (say msg :color color))
       (when default
	 (say "[~A] " default :color color))
       (let* ((input (read-line))
	      (parsed-input (ignore-errors (funcall parser input))))
	 (cond ((and (string-equal input "") default)
		(return default))
	       ((and (string-equal input "") required-p)
		(say "A non empty value is required" :color error-color))
	       ((and (string-equal input "") (not required-p))
		(return nil))
	       ((not parsed-input)
		(if if-invalid
		    (funcall if-invalid)
		    (say "Invalid value" :color error-color)))
	       ((and validator
		     (not (funcall validator parsed-input)))
		(if if-invalid
		    (funcall if-invalid)
		    (say "Invalid value" :color error-color)))
	       (parsed-input
		(return parsed-input))))))

(defun prompt-integer (&optional msg &key default 
				       (required-p t) 
				       if-invalid
				       (color *prompt-color*)
				       (error-color *prompt-error-color*))
  "Prompts for an integer.

   Args: - msg: The prompt.
         - default: Default value. This is returned if the user enters the empty string. Default: nil.
         - required-p(boolean): If T, then the empty string is not allowed as a valid input, and the user is asked again for input. Default: t.
         - if-invalid(function): Function to execute if the validator fails.
         - color: Prompt color
         - error-color: Prompt error color.

   Returns: the entered number"
  (parse-prompt #'parse-integer msg 
		:default default
		:required-p required-p
		:if-invalid (or if-invalid 
				(lambda () (say "Error: Not a number" :color error-color)))
		:color color
		:error-color error-color))

(defun prompt-email (&optional msg &key default 
				     (required-p t) 
				     if-invalid
				     (color *prompt-color*)
				     (error-color *prompt-error-color*))
  "Prompts for an email.

   Args: - msg: The prompt.
         - default: Default value. This is returned if the user enters the empty string. Default: nil.
         - required-p(boolean): If T, then the empty string is not allowed as a valid input, and the user is asked again for input. Default: t.
         - if-invalid(function): Function to execute if the validator fails.
         - color: Prompt color
         - error-color: Prompt error color.

   The email is validated and the process does not stop until the user enters a valid email address.

   Return: the entered email"
  (prompt msg :default default
	  :required-p required-p
	  :validator (clavier:valid-email)
	  :if-invalid (or if-invalid
			  (lambda (&optional value)
			    (say "Invalid email" :color error-color)))
	  :color color
	  :error-color error-color))

(defun prompt-url (&optional msg &key default 
				   (required-p t) 
				   if-invalid
				   (color *prompt-color*)
				   (error-color *prompt-error-color*)
				   probe
				   if-exists
				   (if-does-not-exist :error))
  "Prompts for an url.

   Args: - msg: The prompt.
         - default: Default value. This is returned if the user enters the empty string. Default: nil.
         - required-p(boolean): If T, then the empty string is not allowed as a valid input, and the user is asked again for input. Default: t.
         - if-invalid(function): Function to execute if the validator fails.
         - color: Prompt color
         - error-color: Prompt error color
         - probe(boolean): If T, then url is accessed and verified.
         - if-exists(function): A function to call if the url exists (can be accessed).
         - if-does-not-exist(keyword): One of:
              * :error : Tries again until the url can be accessed.
              * :warn : Warns the user the url could not be accessed and asks for continuing.
              * :warn-and-continue: Warns the user the url could not be accessed and continues.
              * :warn-and-ask-again: Warns the user the url could not be accessed and asks for another url.

   Returns: the entered url"
  (flet ((recurse ()
	   (return-from prompt-url
	     (prompt-url msg :default default
			 :required-p required-p
			 :if-invalid if-invalid
			 :color color
			 :error-color error-color
			 :probe probe
			 :if-exists if-exists
			 :if-does-not-exist if-does-not-exist))))
    (let ((url
	   (prompt msg :default default
		   :required-p required-p
		   :validator (clavier:valid-url)
		   :if-invalid (or if-invalid
				   (lambda (&optional value)
				     (say "Invalid url" :color error-color)))
		   :color color
		   :error-color error-color)))
      (when probe
	(multiple-value-bind (result status)
	    (ignore-errors (drakma:http-request url))
	  (if (member status (list 200 302))
	      (when if-exists
		(funcall if-exists))
	      ;; else
	      (ecase if-does-not-exist
		(:error 
		 (say "The url does not exist." :color error-color)
		 (recurse))
		(:warn
		 (say "The url does not exist." :color error-color)
		 (when (not (ask "Continue?:" :default nil))
		   (recurse)))
		(:warn-and-continue
		 (say "The url does not exist." :color error-color))
		(:warn-and-ask-again
		 (say "The url does not exist." :color error-color)
		 (recurse))))))
	url)))

(defun prompt-pathname (&optional msg &key default 
					(required-p t) 
					if-invalid
					(color *prompt-color*)
					(error-color *prompt-error-color*)
					probe
					if-exists
					(if-does-not-exist :error)
					absolute-p
					file-type
					directory-p)
  "Prompts for a pathname.

   Args: - msg: The prompt.
         - default: Default value. This is returned if the user enters the empty string. Default: nil.
         - required-p(boolean): If T, then the empty string is not allowed as a valid input, and the user is asked again for input. Default: t.
         - if-invalid(function): Function to execute if the validator fails.
         - color: Prompt color
         - error-color: Prompt error color.
         - probe. If T, checks that the file exists on the filesystem.
         - if-exists: Function to call if the probe is successful.
         - if-does-not-exist(keyword): One of:
              * :error : Tries again until the pathname can be accessed.
              * :warn : Warns the user the pathname could not be accessed and asks for continuing.
              * :warn-and-continue: Warns the user the pathname could not be accessed and continues.
              * :warn-and-ask-again: Warns the user the pathname could not be accessed and asks for another pathname."
  (flet ((recurse ()
	   (return-from prompt-pathname
	     (prompt-pathname msg :default default
			      :required-p required-p
			      :if-invalid if-invalid
			      :color color
			      :error-color error-color
			      :probe probe
			      :if-exists if-exists
			      :if-does-not-exist if-does-not-exist
			      :absolute-p absolute-p
			      :file-type file-type))))
    (let ((pathname
	   (parse-prompt #'pathname
			 msg 
			 :default default
			 :required-p required-p
			 :validator (make-instance 'clavier:pathname-validator
						   :absolute-p absolute-p
						   :pathname-type file-type)
			 :if-invalid (or if-invalid
					 (lambda (&optional value)
					   (say "Invalid url" :color error-color)))
			 :color color
			 :error-color error-color)))
      (when probe
	(if (probe-file pathname)
	    (when if-exists
	      (funcall if-exists))
	    ;; else
	    (ecase if-does-not-exist
	      (:error 
	       (say "The pathname does not exist." :color error-color)
	       (recurse))
	      (:warn
	       (say "The pathname does not exist." :color error-color)
	       (when (not (ask "Continue?:" :default nil))
		 (recurse)))
	      (:warn-and-continue
	       (say "The pathname does not exist." :color error-color))
	      (:warn-and-ask-again
	       (say "The pathname does not exist." :color error-color)
	       (recurse)))))
      pathname)))

(defun prompt-datetime (&optional msg &key default 
					(required-p t) 
					if-invalid
					(color *prompt-color*)
					(error-color *prompt-error-color*))
  "Prompts for a timestamp.

   Args: - msg: The prompt.
         - default: Default value. This is returned if the user enters the empty string. Default: nil.
         - required-p(boolean): If T, then the empty string is not allowed as a valid input, and the user is asked again for input. Default: t.
         - if-invalid(function): Function to execute if the validator fails.
         - color: Prompt color
         - error-color: Prompt error color.

   The input is parsed with chronicity library and transformed to a local-time. 
   The input is validated and the process does not stop until the user enters a valid timestamp address.

   Return: the parsed local-time"

  (parse-prompt #'chronicity:parse msg
		:default default
		:required-p required-p
		:if-invalid (or if-invalid
				(lambda () (say "Error. Invalid timestamp"
						:color error-color)))
		:color color
		:error-color error-color))

(defun choose-many (msg options &key if-wrong-option 
				  default 
				  (print-options t)
				  (separator "~%")
				  complete
				  (test #'eql)
				  (color *prompt-color*)
				  (error-color *prompt-error-color*))
  "Asks the user to choose many of the given options.

   Args: - msg(string): The prompt message.
         - options(list): The list of options the user can choose from.
         - if-wrong-option(function): When present, this function is run if the user enters a wrong option. Default: nil.
         - default: The default value. The default value is selected if the user just hits the ENTER key. Default: nil.
         - print-options(boolean): Print the options on the screen. Default: T.
         - separator(string): Separation string to use when printing the options. Default: '~%'
         - complete: If T, then readline completion is enabled. Default: nil.
         - color: Color to use at prompt. Default: *prompt-color*
         - error-color: Color to use when error ocurrs. Default: *prompt-error-color*

  Example: 

  ``(choose-many \"Choose: \" (list \"foo\" \"bar\" \"baz\") :default \"baz\")``

  Tags: menu, choose" 
  (let ((chosen-options nil))
    (flet ((print-options ()
	     (loop 
		for option in options
		for i from 0 
		do
		  (format t "[~A] ~A" i option)
		  (when (< (1+ i) (length options))
		    (format t separator)))
	     (terpri)
	     (say "Chosen options: ~{~A~^, ~}" (reverse chosen-options))
	     (say msg :color color)
	     (when default
	       (say "[~A] " default :color color)))
	   (read-option ()
	     (if complete
		 (progn
		   (rl:register-function :complete (make-completer options))
		   (rl:readline :prompt (format nil "~A~@[[~A]~]" msg default)))
		 (read-line))))
      (when print-options
	(print-options))
      (let* ((chosen-option (read-option))
	     (option-number (ignore-errors (parse-integer chosen-option))))
	(loop 
	   do 
	     (cond ((equalp chosen-option "")
		    (if default
			(return default)
			(return (reverse chosen-options))))
		   ((find chosen-option (mapcar #'princ-to-string options) :test #'string=)
		    (pushnew (find chosen-option (mapcar #'princ-to-string options) :test #'string=) chosen-options :test test)
		    (when print-options
		      (print-options)))
		   ((and option-number
			 (>= option-number 0)
			 (< option-number (length options)))
		    ;; Correct option
		    (pushnew (nth option-number options) chosen-options :test test)
		    (when print-options
		      (print-options)))
		   (t
		    ;; Incorrect option
		    (progn
		      (if if-wrong-option
			  (funcall if-wrong-option)
			  (say "Wrong option." :color error-color))
		      (when print-options
			(print-options)))))
	     (setf chosen-option (read-option))
	     (setf option-number (ignore-errors (parse-integer chosen-option))))))))
