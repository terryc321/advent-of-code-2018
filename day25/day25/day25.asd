;;;; day25.asd

(asdf:defsystem #:day25
  :description "Describe day25 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ( #:uiop #:fiveam
   ;;#:drakma #:cxml
)
  :components ((:file "package")
               (:file "day25")
	       ))


