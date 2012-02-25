(defpackage method-combination-utilities
  (:use #:cl #:closer-mop)
  (:shadowing-import-from #:closer-mop
                          #:standard-class
                          #:standard-generic-function #:defgeneric
                          #:standard-method #:defmethod)
  (:export #:combine-standard-methods
           #:basic
           #:append/nconc
           #:method-combination-expand))

(in-package #:method-combination-utilities)
