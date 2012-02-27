(in-package #:method-combination-utilities)

(defun call-methods (methods)
  "It's rare to see a method combination that doesn't define this method, so
   might as well pull it into this utilities package. CALL-METHODS returns a
   list of CALL-METHOD forms; one for each method in METHODS"
  (mapcar (lambda (method) `(call-method ,method)) methods))

(defun wrap-primary-form
    (primary-form &optional around-methods before-methods after-methods)
  "This is similar to COMBINE-STANDARD-METHODS, but it takes an already-
   generated primary form rather than a list of primary methods. This is because
   it is fairly common for a method combination to deal with primary methods in
   some specialized way, then combine them with normal :AROUND/:BEFORE/:AFTER
   methods. See the BASIC and APPEND/CONC combinations in this package for
   examples."
  (let ((form (if (or before-methods after-methods)
                  `(multiple-value-prog1
                       (progn ,@(call-methods before-methods)
                              ,primary-form)
                     ,@(call-methods after-methods))
                  primary-form)))
    (if around-methods
        `(call-method ,(first around-methods)
                      (,@(rest around-methods) (make-method ,form)))
        form)))

(defun combine-standard-methods
    (primary-methods &optional around-methods before-methods after-methods)
  "It is fairly common practice to take the STANDARD method combination and wrap
   it with additional qualified methods. This encapsulates the standard behavior
   as much as possible to make wrapping simpler. Each argument should be a list 
   of methods to be handled the way the corresponding methods are in the
   STANDARD method combination. It assumes all the lists are ordered properly
   already. See the LAX method combination in this package for an example."
  (wrap-primary-form `(call-method ,(first primary-methods)
                                   ,(rest primary-methods))
                     around-methods
                     before-methods
                     after-methods))
