(in-package #:duologue)

(defparameter *prompt-color* nil)
(defparameter *prompt-error-color* nil)

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
  
(defun say (datum &rest args)
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
  
(defun choose (msg options &key if-wrong-option 
			     default
			     (print-options t)
			     (separator "~%")
			     complete
			     (color *prompt-color*)
			     (error-color *prompt-error-color*))
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
