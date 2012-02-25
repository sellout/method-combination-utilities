(in-package #:method-combination-utilities)

(defun combine-standard-methods (around before primary after)
  "It is fairly common practice to take the STANDARD method combination and wrap
   it with additional qualified methods. This encapsulates the standard behavior
   as much as possible to make wrapping simpler. Each argument should be a list 
   of methods to be handled the way the corresponding methods are in the
   STANDARD method combination. It assumes all the lists are ordered properly
   already."
  (flet ((call-methods (methods)
           (mapcar (lambda (method) `(call-method ,method)) methods)))
    (let ((form (if (or before after (rest primary))
                    `(multiple-value-prog1
                         (progn ,@(call-methods before)
                                (call-method ,(first primary)
                                             ,(rest primary)))
                       ,@(call-methods after))
                    `(call-method ,(first primary)))))
      (if around
          `(call-method ,(first around) (,@(rest around) (make-method ,form)))
          form))))
