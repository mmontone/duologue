(in-package :duologue)

(defun readline/make-list-completer (options)
  "Makes a default completer from a list of options"
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

(defun complete-prompt (prompt options completer)
  (rl:register-function :complete (or completer (readline/make-list-completer options)))
  (string-trim (list #\ ) (rl:readline :prompt prompt)))
