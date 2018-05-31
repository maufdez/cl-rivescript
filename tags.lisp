;;;; tags.lisp

(in-package #:cl-rivescript)

(defun get-curly-var (varname line &optional default)
  "A function to get the weight for a response"
  (let* ((len (length line))
	(rgx (format nil "{\\s*~a\\s*=\\s*(.*?)}" varname))
	(idxs (multiple-value-list (scan rgx line))))
    (if (car idxs)
	(destructuring-bind (match-start match-end vals-start vals-end) idxs
	  (values
	   (if (= match-end len)
	       (string-right-trim *spaces* (subseq line 0 match-start))
	       (concatenate 'string
			       (subseq line 0 match-start)
			       (subseq line match-end)))
	   (let ((value-start (elt vals-start 0))
		 (value-end (elt vals-end 0)))
	     (if (= value-start value-end)
		 default
		 (string-right-trim *spaces* (subseq line value-start value-end))))))
	(values line default))))

(defun getting-curly-var (symbol text)
  "Look for the symbol name as a curly var in the thext and assign the value to it"
  (multiple-value-bind (new-text str-val)
      (get-curly-var
       (string-downcase (symbol-name symbol)) text (symbol-value symbol))
    (setf (symbol-value symbol) str-val)
    new-text))

(defun assigning-curly-vars (varlist text)
  "Apply getting-curly-var iteratively over a list of variables"
  (do ((lst varlist (cdr lst))
       (s text (getting-curly-var (car lst) s)))
      ((null lst) s)))

(defmacro with-curly-vars (defaults &body body)
  "Defines a set of variables and default values to be used by assigning-curly-vars"
  (let ((vars (mapcar #'car defaults)))
    `(let ((vars ',vars),@defaults)
       (declare (special ,@vars))
       ,@body)))

;;; Replace star tags

(defun replace-stars (string star-array)
  "Replace the <starN> tags"
  (multiple-value-bind (match number)(scan-to-strings "<star(\\d)*>" string)
    (if match
      (let ((index (if (elt number 0)
		       (1- (parse-integer (elt number 0) :junk-allowed nil))
		       0)))
	(replace-stars (regex-replace-all match string (elt star-array index)) star-array))
      string)))
