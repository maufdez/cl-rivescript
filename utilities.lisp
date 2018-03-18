;;;; utilities.lisp

(in-package #:cl-rivescript)

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
