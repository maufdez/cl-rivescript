;;;; utilities.lisp

(in-package #:cl-rivescript)

(defparameter *ignore* nil
  "A boolean to ignore multiline comments")

(defvar *spaces* '(#\space #\tab #\return))

(defun toggle-ignore (rawline)
  "Toggles *ignore* if the line contains the righ characters"
  (let* ((line (string-trim *spaces* rawline))
	 (len (length line)))
    (if (and (not *ignore*)
	     (string= (subseq line 0 2) "/*"))
	(setf *ignore* t)
	(when (string= (subseq line (- len 2)) "*/")
	  (setf *ignore* nil)))))

(defun remove-comments (line)
  "Removes the comments and fixes any escaped //"
  (let ((slashpos (position #\/ line :from-end t :test #'char=))
        (fixedline (regex-replace-all "\\\\/" line "/")))
    (if slashpos
	(if (and (> slashpos 1)
		 (char= (aref line (- slashpos 2)) #\\))
	    fixedline
	    (if (char= (aref line (1- slashpos)) #\/)
		(regex-replace-all "\\\\/" (string-right-trim *spaces* (subseq line 0 (1- slashpos))) "/")
		fixedline))
	fixedline)))

(defvar *rivescript-commands* ()
  "This is an alist containing the commands and associated functions")

(defmacro def-rs-command (char lambdalist &body body)
  (let ((f (gensym)))
    `(flet ((,f ,lambdalist
	      ,@body))
       (setf *rivescript-commands* (remove ,char *rivescript-commands* :test #'char= :key #'car))
       (push (cons ,char #',f) *rivescript-commands*))))

(defun clean-string (string)
  (let ((text (string-trim *spaces* string)))
    (format nil "~{~a~^ ~}" (cdr (split "\\s+" text)))))

(defun replace-tags (string)
  "This function replace the space and newline tags in a string"
  (format nil (regex-replace-all "\\\\n" (regex-replace-all "\\\\s" string " ") "~%")))

(defun first-nonspace-char (string)
  "Receives a string and returns the first non space character"
  (aref (remove #\tab (remove #\space string)) 0))

(defun random-elt (list)
  "Randomly select one response from a list"
  (when list (nth (random (length list)) list)))

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

(defun file-ext (file)
  "Receives a file designator and returns anything after the last dot in the name"
  (let* ((name (file-namestring file))
	(dotpos (position #\. name :from-end t :test #'char=)))
    (when dotpos (subseq name (1+ dotpos)))))

(defun has-extension (str file)
  "Receives a string representing the extension
and a file designator, and tells you if
the file has the given extension"
  (string= str (file-ext file)))

(defun do-command (line)
  "Call the appropriate function to process the current line"
  (let* ((cmd-char (first-nonspace-char line))
	 (cmd (assoc cmd-char *rivescript-commands* :test #'char=)))
    (when cmd (funcall (cdr cmd) line))))
