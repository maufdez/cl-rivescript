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

