;;;; cl-rivescript.lisp

(in-package #:cl-rivescript)

(defparameter *brain*
  "+ hello bot
- Hello, human!
+ how are you
- I'm great, how are you?
- I'm good, you?
- Good :) you?
- Great! You?
- I'm fine, thanks for asking!")

(defvar *last-created-trigger* nil
  "A variable to contain the last trigger for easy linking")

(defun add-trigger (string)
  "Adds nodes of type trigger to the database"
  (let ((text (cdr (split "\\s" string))))
    (setf *last-created-trigger* (node-create :label :trigger))
    (setf (get-prop *last-created-trigger* :text) text))) 

(defun add-response (string)
  "Adds a response and ties it to the latest trigger created"
  (let ((text (aref (nth-value 1 (scan-to-strings "\\s*-\\s+(.*)" string)) 0))
        (resp (node-create :label :response)))
    (setf (get-prop resp :text) text)
    (link-create :responds resp *last-created-trigger* :properties '(:weight 1))
    text))

(defun first-nonspace-char (string)
  "Receives a string and returns the first non space character"
  (aref (remove #\tab (remove #\space string)) 0))

(defun do-command (line)
  "Call the appropriate function to process the current line"
  (let ((command (first-nonspace-char line)))
    (case command
      (#\+ (add-trigger line))
      (#\- (add-response line))
      (otherwise nil))))

(defun read-doc (stream)
  "This funciton reads the brain from a stream"
  (loop for line = (read-line stream nil 'eof)
     until (eq line 'eof)
       do (do-command line)))

(defun get-input ()
  "Gets a line of input from the user"
  (mapcar #'(lambda (word) (remove-if-not #'alphanumericp word))
	  (split "\\s+" (string-downcase (read-line)))))

(defun get-answers (input)
  "Gets a list of words forming a phrase to see if it matches any trigger"
  (let ((triggers (node-match :label :trigger :properties `(:text ,input))))
    (if triggers
	(mapcar #'(lambda (trigger) (get-prop trigger :text))
		(rec-search :to triggers :link-type :responds))
	'("I don't understand"))))

(defun select-response (list)
  "Randomly select one response from a list"
  (when list (nth (random (length list)) list)))

(defun main (stream)
  "Get a brain reads it and starts a loop"
  (read-doc stream)
  (loop for input = (get-input) until (string= "bye" (car input))
     do (format t "~a~%" (select-response (get-answers input)))))
