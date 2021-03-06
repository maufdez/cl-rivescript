;;;; utilities.lisp

(in-package #:cl-rivescript)

;;; Variables

(defparameter *ignore* nil
  "A boolean to ignore multiline comments")

(defparameter *inside-label-p* nil
  "A boolean to change processing when iinside a label")

(defvar *spaces* '(#\space #\tab #\return))

(defvar *rs-vars* (make-hash-table :test 'equal)
  "A variable to store the rivescript types")

(defvar *rivescript-commands* ()
  "This is an alist containing the commands and associated functions")

(defvar *exit-label* nil
  "Meant to contain a closure that maintains the open label environment")

(defvar *stars* nil
  "Array containing all of the captured wildcard text")

;;; Utility functions
(defun exit-label ()
  "Function to more clearly execute the exit-label closure"
  (when (functionp *exit-label*)
    (prog1 (funcall *exit-label*)
      (setf *exit-label* nil))))

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
  (let ((acc (reduce #'+ list :key #'cdr)))
    (sort (loop for (val . weight) in list
	     collect (cons val (decf acc weight)))
	  #'> :key #'cdr)))

(defun weighted-random (list)
  "Receives an alist of options and weights and selects accoring to the weights"
  (let* ((len (reduce #'+ list :key #'cdr))
	 (wlist (cumulative-weight (sort list #'< :key #'cdr)))
	 (rnd  (random len)))
    (values (caar (remove-if #'(lambda (n) (>  n rnd)) wlist :key #'cdr))
	    rnd)))

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

(defun get-label-symbol (line)
  "Gets the symbol associated with the label function"
  (cadr (read-from-string (format nil "(~a)" line))))

(defun exec-label-command (line)
  "Executes the function indicated by the label command"
  (let ((args (cddr (split "\\s+" (string-trim *spaces* line)))))
    (apply (symbol-function (get-label-symbol line)) args)))

(defun make-text-capturer ()
  (let ((txt (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
    (list (lambda (line) (with-output-to-string (s txt) (write-line line s)))
	  (lambda () txt))))

(defvar *label-capture* (make-text-capturer))

(defun capture-text (line &optional (text-capturer *label-capture*))
  (funcall (nth 0 text-capturer) line))

(defun get-captured-text (&optional (text-capturer *label-capture*))
  (funcall (nth 1 text-capturer)))


(defun def-rs-type (type)
  "Creates a has with the key of type to store variables of that type"
  (setf (gethash type *rs-vars*)(make-hash-table :test 'equal)))

(defun rs-var (type name)
  "getter for the rivescript variables"
  (gethash name (gethash type *rs-vars*)))

(defun split-lists (ids list)
  "Receives a list of ids and a list to be separated,
creates a list for each id, with the elements that follow it"
  (let ((lists (make-sequence 'list (length ids))))
    (loop
       for e in list
       for b = (position e ids :test #'string=)
       for i = (or b i) unless (or b (not i)) do (push e (nth i lists)))
    (mapcar #'nreverse lists)))

;;; RS Varaiable utilities
(defun set-rs-var (type name value)
  "Setter for the rivescript variables"
  (setf (gethash name (gethash type *rs-vars*)) value))

(defsetf rs-var set-rs-var)

(defun rs-tnv-split (string)
  "Split the name and value"
  (destructuring-bind (type name value)
      (coerce (nth-value 1 (scan-to-strings "^!\\s+(\\w+)\\s+(.*?)\\s*=\\s*(.+)$" string)) 'list)
    (if (string= name "")
	(list "global" type value)
	(list type name value))))

(defun split-rs-array (string)
  "Splits and cleans a rivescript array based on the presence of the | character"
  (let ((separator (or (find #\| string :test #'string=) "s")))
    (mapcar #'(lambda (s) (replace-tags (string-trim *spaces* s)))
	    (split  (format nil "\\~a+" separator) string))))

(defun undef-rs-var (type name)
  "This function undefines a rivescript variable based on the type and name"
  (remhash name (gethash type *rs-vars*)))
;;; End of type variable code

;;; Wildcard processing functions
(defun expand-array (string)
  "Expands a string like (@arrayname)"
  (multiple-value-bind (match-p captured) (scan-to-strings "\\(@(\\w+)\\)" string)
    (if match-p
        (format nil "(~{~a~^|~})" (rs-var "array" (elt captured 0)))
        string)))

(defun convert-wildcard (string)
  "Converts wildcards to equivalent regular expressions"
  (case (char string 0)
    (#\* "(.+)")
    (#\_ "([a-z]+?)")
    (#\# "(\\d+?)")
    (#\( (expand-array string))
    (otherwise string)))

(defun wildcard-indexes (pattern)
  "gets a list of inexes ignoring the optionals"
  (flet ((wc-p (string)(member (char string 0) '(#\* #\# #\_ #\( #\[))))
    (let ((wc-list (remove-if-not #'wc-p pattern)))
      (loop for i from 0 for s in wc-list if (char/= (char s 0) #\[) collect i))))
 
(defun replace-optionals (string)
  "Replaces an rs optional word with the regex equivalent"
  (regex-replace-all
   "\\]\\s" 
   (regex-replace-all "\\s\\[" (concatenate 'string "^ " string " $") "(\\s") "\\s|\\s)"))

(defun to-regex (pattern)
  "converts a match pattern to a regular expression"
  (replace-optionals (format nil "~{~a~^ ~}" (mapcar #'convert-wildcard pattern))))

(defun match-with-pattern (pattern input)
  "Uses regular expressions to match *, _ and # wildcards"
  (let ((wc-idxs (wildcard-indexes pattern)))
    (multiple-value-bind (match-p all-results) (scan-to-strings (to-regex pattern) (format nil " ~{~a~^ ~} " input))
      (when match-p
	(values (string-trim *spaces* match-p)
		(coerce (loop for i in wc-idxs collect (elt all-results i)) 'vector))))))

;; Functions for getting triggers and responses.
(defun get-included-topics (topic)
  "Receives a topic and returns a list of it and all included topics"
  (cons topic (rec-search :from topic :link-type :includes)))

(defun on-topic-triggers (topic-list)
  "Receives a list of topic nodes and retrieves all triggers related to them"
  (node-match :label :trigger :nodes (rec-search :to topic-list :link-type :about)))

(defun matching-input (trigger-list input)
  "Recursive function to form a list of matched triggers and captured text"
  (when (car trigger-list)
    (multiple-value-bind (matched captured)
	(match-with-pattern (get-prop (car trigger-list) :text) input)
      (if matched (cons (cons (car trigger-list) captured)
			(matching-input (cdr trigger-list) input))
	  (matching-input (cdr trigger-list) input)))))

(defun has-conditionals (trigger)
  "Receives a trigger text property and returns nil if it has no conditionals"
  (loop
     for word in (get-prop trigger :text)
     for result = (find #\[ word :test #'char=)
     until result finally (return result)))

(defun has-no-stars (trigger-matches)
  "Receives a structure of trigger-matches like the one
from matching-input, and returns those with empty stars vector"
  (remove-if-not #'(lambda (stars) (zerop (length stars))) trigger-matches :key #'cdr))

(defun non-wc-words (trigger-match)
  "Receives one of the elements of a trigger match list
returns the number of words which are not wildcards"
  (when (car trigger-match)
    (- (length (get-prop (car trigger-match) :text))
       (length (cdr trigger-match)))))

(defun count-nwc-chars (trigger-match)
  "Receives a trigger match and returns the number or character
which are not wildcards, alternations or optionals"
  (when (car trigger-match)
    (let ((text (get-prop (car trigger-match) :text)))
      (reduce #'+
	      (mapcar #'length
		      (remove-if #'(lambda (s) (scan "[\\(\\[\\*_#]" s)) text))))))

(defun trigger-has (char trigger-match)
  "Checks for a particular character to see if it is present"
  (let ((text (get-prop (car trigger-match) :text)))
    (loop for token in text for r = (find char token :test #'char=) finally (return r))))

(defun >-wildcard (trigger-match1 trigger-match2)
  "Applies _ > # > * ordering"
  (or (and (trigger-has #\_ trigger-match1)
	   (not (trigger-has #\_ trigger-match2)))
      (and (trigger-has #\# trigger-match1)
	   (not (trigger-has #\# trigger-match2)))))

(defun compare-trigger-matches (predicate property)
  "Compares the text properties of two trigger match elements
and returns true if the first is greater than the second"
  (lambda (tm1 tm2)
    (funcall predicate
	     (funcall property tm1)
	     (funcall property tm2))))

(defun top-trigger-matches (predicate property trigger-matches)
  "Receives a list of trigger matches and keeps only the longest
by wordcount"
  (let* ((sorter (compare-trigger-matches predicate property))
	 (sorted (sort trigger-matches sorter))
         (max-len (funcall property (first sorted))))
    (loop for tm in sorted while (= max-len (funcall property tm)) collect tm)))

(defmacro piping (&body body)
  (reduce #'(lambda (x y) (reverse (list x y))) `(,@body)))

;; Evaluator
(defun do-command (line)
  "Call the appropriate function to process the current line"
  (let* ((cmd-char (first-nonspace-char line))
	 (cmd (assoc cmd-char *rivescript-commands* :test #'char=)))
    (if (and *inside-label-p* (char/= cmd-char #\<))
	(capture-text line)
	(when cmd (funcall (cdr cmd) line)))))
