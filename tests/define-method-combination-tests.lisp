(in-package #:method-combination-utilities.tests)

(def-suite define-method-combination)

(def-suite spec-examples :in define-method-combination)

(in-suite spec-examples)

(test should-create-short-and-combination
  (is-true (define-method-combination short-and
               :identity-with-one-argument t)))

(test should-create-long-and-combination
  (is-true (define-method-combination long-and 
               (&optional (order :most-specific-first))
             ((around (:around))
              (primary (and) :order order :required t))
             (let ((form (if (rest primary)
                             `(and ,@(mapcar #'(lambda (method)
                                                 `(call-method ,method))
                                             primary))
                             `(call-method ,(first primary)))))
               (if around
                   `(call-method ,(first around)
                                 (,@(rest around)
                                    (make-method ,form)))
                   form)))))

(test should-create-standard-combination
  (is-true (define-method-combination test-standard ()
             ((around (:around))
              (before (:before))
              (primary () :required t)
              (after (:after)))
             (flet ((call-methods (methods)
                      (mapcar #'(lambda (method)
                                  `(call-method ,method))
                              methods)))
               (let ((form (if (or before after (rest primary))
                               `(multiple-value-prog1
                                    (progn ,@(call-methods before)
                                           (call-method ,(first primary)
                                                        ,(rest primary)))
                                  ,@(call-methods (reverse after)))
                               `(call-method ,(first primary)))))
                 (if around
                     `(call-method ,(first around)
                                   (,@(rest around)
                                      (make-method ,form)))
                     form))))))

(test should-create-simple-or-combination
  (is-true (define-method-combination simple-or ()
             ((methods (or)))
             `(or ,@(mapcar #'(lambda (method)
                                `(call-method ,method))
                            methods)))))

(test should-create-more-complete-or-combination
  (is-true (define-method-combination more-complete-or 
               (&optional (order ':most-specific-first))
             ((around (:around))
              (primary (or)))
             ;; Process the order argument
             (case order
               (:most-specific-first)
               (:most-specific-last (setq primary (reverse primary)))
               (otherwise (method-combination-error "~S is an invalid order.~@
          :most-specific-first and :most-specific-last are the possible values."
                                                    order)))
             ;; Must have a primary method
             (unless primary
               (method-combination-error "A primary method is required."))
             ;; Construct the form that calls the primary methods
             (let ((form (if (rest primary)
                             `(or ,@(mapcar #'(lambda (method)
                                                `(call-method ,method))
                                            primary))
                             `(call-method ,(first primary)))))
               ;; Wrap the around methods around that form
               (if around
                   `(call-method ,(first around)
                                 (,@(rest around)
                                    (make-method ,form)))
                   form)))))

(test should-create-or-combination-with-options
  (is-true (define-method-combination or-with-options
               (&optional (order ':most-specific-first))
             ((around (:around))
              (primary (or) :order order :required t))
             (let ((form (if (rest primary)
                             `(or ,@(mapcar #'(lambda (method)
                                                `(call-method ,method))
                                            primary))
                             `(call-method ,(first primary)))))
               (if around
                   `(call-method ,(first around)
                                 (,@(rest around)
                                    (make-method ,form)))
                   form)))))

(test should-create-short-or-combination
  (is-true (define-method-combination short-or
               :identity-with-one-argument t)))



(test should-create-example-combination
  (is-true (flet ((positive-integer-qualifier-p (method-qualifiers)
                    (and (= (length method-qualifiers) 1)
                         (typep (first method-qualifiers) '(integer 0 *)))))
             (define-method-combination example-method-combination ()
               ((methods positive-integer-qualifier-p))
               `(progn ,@(mapcar #'(lambda (method)
                                     `(call-method ,method))
                                 (stable-sort methods #'<
                                              :key #'(lambda (method)
                                                       (first (method-qualifiers method))))))))))

(test should-create-progn-with-lock-combination
  (is-true (define-method-combination progn-with-lock ()
             ((methods ()))
             (:arguments object)
             `(unwind-protect
                   (progn (lock (object-lock ,object))
                          ,@(mapcar #'(lambda (method)
                                        `(call-method ,method))
                                    methods))
                (unlock (object-lock ,object))))))

(def-suite additional-examples :in define-method-combination)

(in-suite additional-examples)

(test should-create-combination-with-&whole
  (is-true (define-method-combination progn-with-whole ()
             ((methods ()))
             (:arguments &whole whole)
             (format nil "using ~a" whole)
             `(progn ,@(mapcar #'(lambda (method)
                                   `(call-method ,method))
                               methods))))
  (finishes (defgeneric test-&whole (x)
              (:method-combination progn-with-whole)
              (:method (x) x)))
  (is (= 4 (test-&whole 4))))
