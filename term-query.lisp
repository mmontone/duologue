(in-package #:term-query)

(defun msg (msg &rest args)
  (apply #'format t (cons msg args))
  (terpri t))

(defun msg* (msg &rest args)
  (apply #'format t (cons msg args)))  

(defun choose (msg options &key if-wrong-option default (separator "~%"))
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
	     (msg* "[~A] " default))))
    (render-options)
    (let* ((chosen-option (read-line))
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
	   (setf chosen-option (read-line))
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

(defun prompt (&optional msg &key default)
  (when msg
    (msg* msg))
  (when default
    (format t "[~A] " default))
  (let ((input (read-line)))
    (if (and (string-equal input "") default)
	default
	input)))

(defun prompt-integer (&optional msg &key default if-wrong-input)
  (flet ((render-input ()
	   (when msg
	     (msg* msg))
	   (when default
	     (format t "[~A] " default))))
    (loop do
	 (render-input)
	 (let* ((input (read-line))
		(number (ignore-errors (parse-integer input))))
	   (cond 
	     ((and (string-equal input "") default)
	      (return default))
	     ((not number)
	      (if if-wrong-input
		  (funcall if-wrong-input)
		  (msg "Error: Not a number")))
	     (number
	      (return number)))))))
