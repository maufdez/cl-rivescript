;;;; cl-rivescript.asd

(asdf:defsystem #:cl-rivescript
  :description "Describe cl-rivescript here"
  :author "Mauricio Fernandez <maufdez2@gmail.com>"
  :license  "mit"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre #:cl-protograph)
  :components ((:file "package")
               (:file "cl-rivescript")))
