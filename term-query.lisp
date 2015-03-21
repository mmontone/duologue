(in-package #:term-query)

(defun msg (msg &rest args)
  (apply #'format t (cons msg args))
  (terpri t))

(defun msg* (msg &rest args)
  (apply #'format t (cons msg args)))

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
			     (separator "~%")
			     complete)
  (flet ((render-options ()
	   (loop 
	      for option in options
	      for i from 0 
	      do
		(format t "[~A] ~A" i option)
		(when (< (1+ i) (length options))
		  (format t separator)))
	   (terpri)
	   (msg* msg)
	   (when default
	     (msg* "[~A] " default)))
	 (read-option ()
	   (if complete
	       (progn
		 (rl:register-function :complete (make-completer options))
		 (rl:readline :prompt (format nil "~A~@[[~A]~]" msg default)))
	       (read-line))))
    (render-options)
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
			(msg "Wrong option."))
		    (render-options))))
	   (setf chosen-option (read-option))
	   (setf option-number (ignore-errors (parse-integer chosen-option)))))))
  
(defun ask (&optional (msg "Yes or no: ") &key (default nil default-p) if-wrong-answer)
  (check-type default boolean)
  (labels ((format-boolean (boolean)
	     (if boolean "yes" "no"))
	   (ask-question ()
	     (format t msg)
	     (when default-p
	       (format t "[~A] " (format-boolean default)))))
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
		  (msg "Answer yes or no"))
	      (ask-question)
	      (setf answer (read-line))))))))

(defun prompt (&optional msg &key default (required-p t) validator if-invalid)
  (loop do
       (when msg
	 (msg* msg))
       (when default
	 (msg* "[~A] " default))
       (let ((input (read-line)))
	 (cond ((and (string-equal input "") default)
		(return default))
	       ((and (string-equal input "") required-p)
		(msg "A non empty value is required"))
	       ((and validator
		     (not (funcall validator input)))
		(if if-invalid
		    (funcall if-invalid input)
		    (msg "The value is not valid")))
	       (t
		(return input))))))

(defun parse-prompt (parser &optional msg &key default (required-p t) if-wrong-input)
  (loop do
       (when msg
	 (msg* msg))
       (when default
	 (msg* "[~A] " default))
       (let* ((input (read-line))
	      (parsed-input (ignore-errors (funcall parser input))))
	 (cond ((and (string-equal input "") default)
		(return default))
	       ((and (string-equal input "") required-p)
		(msg "A non empty value is required"))
	       ((and (string-equal input "") (not required-p))
		(return nil))
	       ((not parsed-input)
		(if if-wrong-input
		    (funcall if-wrong-input)
		    (msg "Invalid value")))
	       (parsed-input
		(return parsed-input))))))	       

(defun prompt-integer (&optional msg &key default (required-p t) if-wrong-input)
  (parse-prompt #'parse-integer msg 
		:default default
		:required-p required-p
		:if-wrong-input (or if-wrong-input 
				    (lambda () (msg "Error: Not a number")))))	

(defun prompt-email (&optional msg &key default (required-p t) if-wrong-input)
  (prompt msg :default default
	  :required-p required-p
	  :validator (clavier:valid-email)
	  :if-invalid (or if-wrong-input
			  (lambda (&optional value)
			    (msg "Invalid email")))))

(defun prompt-url (&optional msg &key default (required-p t) if-wrong-input)
  (prompt msg :default default
	  :required-p required-p
	  :validator (clavier:valid-url)
	  :if-invalid (or if-wrong-input
			  (lambda (&optional value)
			    (msg "Invalid url")))))

(defun prompt-datetime (&optional msg &key default (required-p t) if-wrong-input)
  (parse-prompt #'chronicity:parse msg
		:default default
		:required-p required-p
		:if-wrong-input (or if-wrong-input
				    (lambda () (msg "Error. Invalid timestamp")))))

(defun choose-many (msg options &key if-wrong-option default (separator "~%") (test #'eql))
  (let ((chosen-options nil))
    (flet ((render-options ()
	     (loop 
		for option in options
		for i from 0 
		do
		  (format t "[~A] ~A" i option)
		  (when (< (1+ i) (length options))
		    (format t separator)))
	     (terpri)
	     (msg "Chosen options: ~{~A~^, ~}" (reverse chosen-options))
	     (msg* msg)
	     (when default
	       (msg* "[~A] " default))))
      (render-options)
      (let* ((chosen-option (read-line))
	     (option-number (ignore-errors (parse-integer chosen-option))))
	(loop 
	   do 
	     (cond ((equalp chosen-option "")
		    (if default
			(return default)
			(return (reverse chosen-options))))
		   ((find chosen-option (mapcar #'princ-to-string options) :test #'string=)
		    (pushnew (find chosen-option (mapcar #'princ-to-string options) :test #'string=) chosen-options :test test)
		    (render-options))
		   ((and option-number
			 (>= option-number 0)
			 (< option-number (length options)))
		    ;; Correct option
		    (pushnew (nth option-number options) chosen-options :test test)
		    (render-options))
		   (t
		    ;; Incorrect option
		    (progn
		      (if if-wrong-option
			  (funcall if-wrong-option)
			  (msg "Wrong option."))
		      (render-options))))
	     (setf chosen-option (read-line))
	     (setf option-number (ignore-errors (parse-integer chosen-option))))))))
