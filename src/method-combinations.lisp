(in-package #:method-combination-utilities)

(define-method-combination primary ()
  ((primary () :required t))
  "This simple method combination requires that no methods have any qualifiers.
   Methods are treated exactly like primary methods in the STANDARD method
   combination."
  `(call-method ,(first primary) ,(rest primary)))

(define-method-combination lax ()
  ((around (:around . *))
   (before (:before . *))
   (after (:after . *) :order :most-specific-last)
   (primary * :required t))
  "This combination allows (and ignores) additional method qualifiers after any
   of the `STANDARD` qualifiers. It is useful if you are handling your
   qualifiers in a custom method class."
  (combine-standard-methods primary around before after))

(define-method-combination basic
    (operator
     &optional identity-with-one-argument-p (order :most-specific-first))
  ((around (:around))
   (before (:before))
   (after (:after))
   (primary (*) :order order :required t))
  "This combination removes the need for the built-in combinations (other than
   STANDARD) and the short form of DEFINE-METHOD-COMBINATION.

   Differences from old built-in usage:
 * specify as '(basic progn t :most-specific-last) instead of
  '(progn :most-specific-last)
 * can specify an arbitrary operator without having to first
   DEFINE-METHOD-COMBINATION
 * IDENTITY-WITH-ONE-ARGUMENT[-P] is now specified at usage rather than
   definition
 * :BEFORE and :AFTER methods are allowed."
  (let ((invalid-method (find-if-not (lambda (qualifiers)
                                       (equal (list operator) qualifiers))
                                     primary
                                     :key #'method-qualifiers)))
    (when invalid-method
      (error "~S is an invalid method.~@
              Its qualifier must be either :AROUND or ~S."
             invalid-method operator)))
  (wrap-primary-form (if (or (not identity-with-one-argument-p) (rest primary))
                         `(,operator ,@(call-methods primary))
                         `(call-method ,(first primary)))
                     around
                     before
                     after))

(define-method-combination append/nconc (&optional order)
  ((around (:around))
   (primary (append) (nconc) :order order :required t))
  "This is an improvement on the built-in APPEND method combination, which uses
   the short form of DEFINE-METHOD-COMBINATION. This allows either `NCONC` or
  `APPEND` to be used as a qualifier depending on whether or not the method
   returns a list where the last cons can be modified. If all but the last
   primary methods specify `NCONC`, then `NCONC` will be used, otherwise,
  `APPEND`."
  (wrap-primary-form (if (rest primary)
                         `(,(if (find '(append) (butlast primary)
                                      :test #'equal :key #'method-qualifiers)
                                'append
                                'nconc)
                            ,@(call-methods primary))
                         `(call-method ,(first primary)))
                     around))
