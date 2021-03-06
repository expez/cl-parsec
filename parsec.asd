;;; parsec.asd

(asdf:defsystem #:parsec
  :name "parsec"
  :author "Alex Suraci <i.am@toogeneric.com>"
  :version "1.0"
  :maintainer "Alex Suraci <i.am@toogeneric.com>"
  :license "BSD"
  :description "Parser combinators library based on Haskell's Parsec library."
  :serial t
  :depends-on (#:cl-unicode)
  :in-order-to ((test-op (load-op :parsec-tests)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN!" :parsec-tests)
                             :parsec-tests))
  :components ((:file "package")
               (:file "unicode")
               (:file "data")
               (:file "parsec")
               (:file "primitive")
               (:file "char")
               (:file "combinators")))

(asdf:defsystem #:parsec-tests
  :depends-on (#:parsec #:fiveam)
  :components ((:file "test")))
