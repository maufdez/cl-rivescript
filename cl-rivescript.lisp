;;;; cl-rivescript.lisp

(in-package #:cl-rivescript)

;;; Global variables

(defvar *depth* 25
  "Recursion limit before an attempt to fetch a reply will be abandoned.")

(defvar *last-created-trigger* nil
  "A variable to contain the last trigger for easy linking")

(defvar *last-created-response* nil
  "A variable to contain the last response for easy linking")

(defvar *default-topic*
  (node-create :label :topic :properties '(:text "random"))
  "Default topic node")

(defvar *default-answer*
  (node-create :label :response
	       :properties '(:text "I do not understand" :new-topic "random")))

(defvar *current-topic* *default-topic*
  "Points to the node currently being defined")

(defvar *user-topic* *default-topic*
  "Points to the user's conversation topic")

(defvar *prev-rs-command* #\space
  "It is set by commands that accept continuation")

(defvar *last-array-var* nil
  "This variable stores the name of the last array that was accessed")

;;; Declaration of RiveScript variable types
(def-rs-type "global")

(def-rs-type "var")

(def-rs-type "array")

(def-rs-type "sub")

(def-rs-type "person")

(def-rs-type "local")

;;; Topic related functions
(defun switch-topic (text &key (create nil))
  "Switch to a different topic"
  (let ((topic-node (car (node-match :label :topic :properties `(:text ,text)))))
    (if topic-node
	(setf *current-topic* topic-node)
	(if create
	    (setf *current-topic* (node-create :label :topic :properties `(:text ,text)))
	    (setf *current-topic* *default-topic*)))))

(defun topic (&rest args)
  "Define the contents of a conversation topic"
  (when args
    (let ((topic (switch-topic (car args) :create t))
	  (includes (if (string= (cadr args) "includes")
			(cddr args)
			nil)))
      (with-input-from-string (s (get-captured-text))
	(loop for line = (read-line s nil 'eof)
	   until (eq line 'eof)
	   do (process-line line))
	(loop for i from 1 for inc-topic in includes
	   do (link-create :includes topic (switch-topic inc-topic :create t) :properties `(:order ,i))))))
  (setf *current-topic* *default-topic*))

;;; Helper functions for the command definitions
(defun continue-response (string)
  "Function to continue a multi-line response definition"
  (let ((current-text (get-prop *last-created-response* :text))
	(new-text (clean-string string)))
    (setf (get-prop *last-created-response* :text)
	  (replace-tags (format nil "~a~a" current-text new-text)))))

(defun continue-array (string)
  "Function to continua a multi-line array definition"
  (let ((text (clean-string string))
	(original-array (copy-seq (rs-var "array" *last-array-var*))))
    (setf (rs-var "array" *last-array-var*) (append original-array (split-rs-array text)))))

;;; Definition of RiveScript commands

;; Trigger Command
(def-rs-command #\+ (string)
  (let ((text (cdr (split "\\s" string))))
    (setf *last-created-trigger* (node-create :label :trigger))
    (link-create :about *last-created-trigger* *current-topic*)
    (setf (get-prop *last-created-trigger* :text) text))) 

;; Response or Reply Command
(def-rs-command #\- (string)
  (prog1
      (with-curly-vars
	  ((weight "1")
	   (topic nil))
	(let* ((text (clean-string (assigning-curly-vars vars string)))
	       (resp (node-create :label :response))
	       (lnk (link-create :responds resp *last-created-trigger*)))
	  (setf (get-prop resp :text) text)
	  (when topic (setf (get-prop resp :new-topic) topic))
	  (setf (get-prop lnk :weight)(parse-integer weight :junk-allowed t))
	  (link-create :about resp *current-topic*)
	  (setf *last-created-response* resp)
	  text))
    (setf *prev-rs-command* #\-)))

;; Continue command
(def-rs-command #\^ (string)
  (case *prev-rs-command*
    (#\- (continue-response string))
    (#\! (continue-array string))
    (otherwise nil)))

;; Label block start command
(def-rs-command #\> (string)
  (setf *label-capture* (make-text-capturer))
  (setf *inside-label-p* t)
  (setf *exit-label* (lambda () (exec-label-command string))))

;; Label block finish command
(def-rs-command #\< (string)
  (declare (ignorable string))
  (setf *inside-label-p* nil)
  (exit-label)
  (setf *label-capture* nil))

;; Declaration command
(def-rs-command #\! (string)
  (prog1 
      (let ((text (string-trim *spaces* string)))
	(multiple-value-bind (result strings)(scan-to-strings "^!\\s+(\\w+)" text)
	  (let ((type (if result (elt strings 0) "")))
	    (when (and result (or (string= type "version")
				  (gethash type *rs-vars*)))
	      (destructuring-bind (type name value)(rs-tnv-split text)
		(if (string= value "<undef>")
		    (undef-rs-var type name)
		    (setf (rs-var type name)(if (string= type "array")
						(progn
						  (setf *last-array-var* name)
						  (split-rs-array value))
						value))))))))
    (setf *prev-rs-command* #\!)))

;;; RiveScript file related functions

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

(defun included-topic (order list)
  "Receives an integer and a list of included topics,
returns the node with the matching :order property"
  (to-node (car (link-match :properties `(:order ,order) :links list))))

(defun get-answers (input &optional (topic *user-topic*) (depth 1) hist)
  "Gets a list of words forming a phrase to see if it matches any trigger"
  (let* ((nodes-about (rec-search :to topic :link-type :about))
         (triggers (node-match :label :trigger
                               :properties `(:text ,input)
                               :nodes nodes-about))
         (response-links  (link-search :from nodes-about :link-type :responds))
	 (topic-text (get-prop topic :text))
	 (topics (link-search :from topic :link-type :includes)))
    (if triggers
        (mapcar #'(lambda (link) (cons (from-node link)
                                       (get-prop link :weight)))
                (link-search :to triggers :link-type :responds
                             :link-list response-links))
        (if (or (member topic-text hist) (> (1+ depth) *depth*))
	    nil
	    (when topics
	      (loop
		 for i from 1 upto (length topics)
		 for responses = (get-answers input
					      (included-topic i topics)
					      (1+ depth) (cons topic-text hist))
		 when responses
		 return responses))))))

(defun select-response (list)
  (when list
    (let* ((response-node  (weighted-random list))
	   (text (get-prop response-node :text))
	   (new-topic (get-prop response-node :new-topic)))
      (when new-topic
	(setf *user-topic* (switch-topic new-topic :create nil)))
      text)))

;;; Entry points
(defun main (directory)
  "Receives a brain directory and processes all .rive files"
  (read-line)
  (walk-directory directory #'read-doc :test #'rivescript-doc-p)
  (loop for input = (progn
		      (format t "~&> ")
		      (force-output)
		      (get-input))
     until (string= "bye" (car input))
     do (format t "~a~%" (select-response (or (get-answers input)
					      (list (cons *default-answer* 1)))))))

