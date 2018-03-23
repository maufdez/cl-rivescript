;;;; cl-rivescript.asd

(asdf:defsystem #:cl-rivescript
  :description "RiveScript 2.0 interpreter"
  :author "Mauricio Fernandez <maufdez2@gmail.com>"
  :license  "mit"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-fad #:cl-ppcre #:cl-protograph)
  :components ((:file "package")
	       (:file "utilities" :depends-on ("package"))
	       (:file "tags" :depends-on ("package"))
               (:file "cl-rivescript" :depends-on ("package" "utilities" "tags"))))
