(in-package #:term-query)

(defun msg (msg &rest args)
  (apply #'format t (cons msg args))
  (terpri t))

(defun msg* (msg &rest args)
  (apply #'format t (cons msg args)))  

(defun choose (msg options &key if-wrong-option (separator "~%"))
  (flet ((render-options ()
	   (loop 
	      for option in options
	      for i from 0 
	      do
		(format t "[~A] ~A" i option)
		(when (< (1+ i) (length options))
		  (format t separator)))
	   (terpri)
	   (msg* msg)))
    (render-options)
    (let ((chosen-option (ignore-errors (parse-integer (read-line)))))
      (loop 
	 do 
	   (if (and chosen-option
		    (>= chosen-option 0)
		    (< chosen-option (length options)))
	       ;; Correct option
	       (return)
	       ;; Incorrect option
	       (progn
		 (if if-wrong-option
		     (funcall if-wrong-option)
		     (msg "Wrong option."))
		 (render-options)))
	   (setf chosen-option (ignore-errors (parse-integer (read-line)))))
      (values (nth chosen-option options) chosen-option))))

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
