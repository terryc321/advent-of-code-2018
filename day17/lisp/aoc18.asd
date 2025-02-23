;;;; aoc18.asd

(asdf:defsystem #:aoc18
  :description "Describe aoc18 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
               (:file "fun")))
