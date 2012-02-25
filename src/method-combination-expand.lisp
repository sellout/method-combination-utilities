(in-package #:method-combination-utilities)

(defun method-combination-expander-function (method-combination)
  #+ccl (cdr (ccl::method-combination-expander method-combination))
  #-ccl (error "this function is not available on ~A"
               (lisp-implementation-type)))

(defun method-combination-options (method-combination)
  #+ccl (ccl::method-combination-options method-combination)
  #-ccl (error "this function is not available on ~A"
               (lisp-implementation-type)))

(defmacro method-combination-expand (form)
  "Given a function call form, this returns the method-combination form that
   would be generated in the process of satisfying that call. This is only
   expected to work for functions whose method-combination was created using
   the long form of DEFINE-METHOD-COMBINATION.

   NOTE: This is currently CCL-only."
  (let* ((gf (symbol-function (car form)))
         (mc (generic-function-method-combination gf)))
    `(funcall ,(method-combination-expander-function mc)
              ,gf
              (compute-applicable-methods ,gf (list ,@(cdr form)))
              (method-combination-options ,mc))))
