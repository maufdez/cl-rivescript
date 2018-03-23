;;;; cl-rivescript.lisp

(in-package #:cl-rivescript)

(defvar *last-created-trigger* nil
  "A variable to contain the last trigger for easy linking")

(defvar *last-created-response* nil
  "A variable to contain the last response for easy linking")

(def-rs-command #\+ (string)
  (let ((text (cdr (split "\\s" string))))
    (setf *last-created-trigger* (node-create :label :trigger))
    (setf (get-prop *last-created-trigger* :text) text))) 

(def-rs-command #\- (string)
  (multiple-value-bind (newstring weight-str) (get-curly-var "weight" string "1")
    (let* ((text (clean-string newstring))
	   (resp (node-create :label :response))
	   (lnk (link-create :responds resp *last-created-trigger*)))
      (setf (get-prop resp :text) text)
      (setf (get-prop lnk :weight)(parse-integer weight-str :junk-allowed t))
      (setf *last-created-response* resp)
      text)))

(def-rs-command #\^ (string)
  (let ((current-text (get-prop *last-created-response* :text))
	(new-text (clean-string string)))
    (setf (get-prop *last-created-response* :text)
	  (replace-tags (format nil "~a~a" current-text new-text)))))

(defun rivescript-doc-p (file)
  "Tests if a given file is a rive document based on the extension"
  (has-extension "rive" file))

(defun process-line (line)
  "Does the appropriate actions based on the line contents"
  (let ((string (remove-comments (string-trim *spaces* line))))
    (unless (string= "" string)
      (toggle-ignore string)
      (unless *ignore* (do-command string))
      (toggle-ignore string)
      string)))

(defun read-doc (file)
  "This function reads the brain from a stream"
  (with-open-file (stream file :direction :input)
    (loop for line = (read-line stream nil 'eof)
       until (eq line 'eof)
       do (process-line line))))

(defun get-input ()
  "Gets a line of input from the user"
  (mapcar #'(lambda (word) (remove-if-not #'alphanumericp word))
	  (split "\\s+" (string-downcase (read-line)))))

(defun get-answers (input)
  "Gets a list of words forming a phrase to see if it matches any trigger"
  (let ((triggers (node-match :label :trigger :properties `(:text ,input))))
    (if triggers
	(mapcar #'(lambda (link) (cons (get-prop (proto-graph::from-node link) :text)
				       (get-prop link :weight)))
		(link-search :to triggers :link-type :responds))
	'(("I don't understand" . 1)))))

(defun select-response (list)
  "Randomly select one response from a list"
  (when list (weighted-random list)))

(defun main (directory)
  "Receives a brain directory and processes all .rive files"
  (read-line)
  (walk-directory directory #'read-doc :test #'rivescript-doc-p)
  (loop for input = (progn
		      (format t "~&> ")
		      (force-output)
		      (get-input))
     until (string= "bye" (car input))
     do (format t "~a~%" (select-response (get-answers input)))))

