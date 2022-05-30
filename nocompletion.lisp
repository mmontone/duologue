(in-package :duologue)

(defun complete-prompt (prompt options completer)
  (declare (ignore prompt options completer))
  (error "duologue error: no completion backend loaded"))
