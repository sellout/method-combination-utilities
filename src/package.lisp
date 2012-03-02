(defpackage method-combination-utilities
  (:use #+abcl #:cl #-abcl #:closer-common-lisp #-abcl #:closer-mop)
  (:export #:call-methods #:wrap-primary-form #:combine-standard-methods
           #:primary #:lax #:basic #:append/nconc
           #:method-combination-expand))

(in-package #:method-combination-utilities)
