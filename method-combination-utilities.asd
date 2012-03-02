(defpackage method-combination-utilities.system
  (:use #:cl #:asdf))

(in-package #:method-combination-utilities.system)

(defsystem method-combination-utilities
  :description "Various method combinations and utilities to make it easy to
                create new method combinations."
  :long-description "See README.md"
  :author "Greg Pfeil <greg@technomadic.org>"
  :license "MIT"
  :depends-on (#-abcl closer-mop)
  :pathname "src/"
  :components ((:file "package")
               (:file "definition-helpers" :depends-on ("package"))
               (:file "method-combinations" :depends-on ("package"))
               (:file "method-combination-expand" :depends-on ("package")))
  :in-order-to ((test-op (load-op method-combination-utilities.tests)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN!" :list-it.tests)
                             (intern "TESTS" :list-it.tests))))

(defmethod operation-done-p
    ((op test-op) (c (eql (find-system '#:method-combination-utilities))))
  (values nil))

(defsystem method-combination-utilities.tests
  :author "Greg Pfeil <greg@technomadic.org>"
  :license "MIT"
  :depends-on (method-combination-utilities fiveam)
  :pathname "tests/"
  :components ((:file "package")
               (:file "definition-helpers" :depends-on ("package"))
               (:file "method-combinations" :depends-on ("package"))
               (:file "method-combination-expand" :depends-on ("package"))))
