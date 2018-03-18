;;;; utilities.lisp

(in-package #:cl-rivescript)

(defvar *rivescript-commands* ()
  "This is an alist containing the commands and associated functions")

(defmacro def-rs-command (char lambdalist &body body)
  (let ((f (gensym)))
    `(flet ((,f ,lambdalist
	      ,@body))
       (pushnew (cons ,char #',f) *rivescript-commands* :key #'car))))

(defun clean-string (string)
  (let ((text (string-trim '(#\space #\tab) string)))
    (format nil "~{~a~^ ~}" (cdr (split "\\s+" text)))))

(defun replace-tags (string)
  "This function replace the space and newline tags in a string"
  (format nil (regex-replace-all "\\\\n" (regex-replace-all "\\\\s" string " ") "~%")))

(defun first-nonspace-char (string)
  "Receives a string and returns the first non space character"
  (aref (remove #\tab (remove #\space string)) 0))

(defun cumulative-weight (list)
  "Returns a list with the cumulative weights"
  (do ((wlist list (cdr wlist))
       (acc (cdar list) (+ acc (cdar wlist)))
       (result nil (cons (cons (caar wlist) (1- acc)) result)))
      ((null wlist) result)))

(defun weighted-random (list)
  "Receives an alist of options and weights and selects accoring to the weights"
  (let* ((len (reduce #'+ list :key #'cdr))
	 (wlist (cumulative-weight list))
	 (rnd  (random len)))
    (caar (remove-if #'(lambda (n) (> n rnd)) wlist :key #'cdr))))

(defun do-command (line)
  "Call the appropriate function to process the current line"
  (let* ((cmd-char (first-nonspace-char line))
	 (cmd (assoc cmd-char *rivescript-commands* :test #'char=)))
    (when cmd (funcall (cdr cmd) line))))
