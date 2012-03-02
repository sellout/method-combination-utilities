(in-package #:method-combination-utilities)

(defun method-combination-expander-function (method-combination)
  (declare (ignorable method-combination))
  #+ccl (cdr (ccl::method-combination-expander method-combination))
  #+clisp (clos::method-combination-expander method-combination)
  #+cmucl (slot-value method-combination 'pcl::function)
  #+sbcl (gethash (sb-pcl::method-combination-type-name method-combination)
                  sb-pcl::*long-method-combination-functions*)
  #-(or ccl clisp cmucl sbcl)
  (error "this function is not available on ~A" (lisp-implementation-type)))

(defun method-combination-expansion-form (expander gf mc methods)
  (declare (ignorable expander gf mc methods))
  #+ccl `(funcall ,expander ,gf ,methods ,(ccl::method-combination-options mc))
  #+clisp `(values (funcall ,expander
                            ,gf
                            ,mc
                            ,(clos::method-combination-options mc)
                            ,methods))
  #+(or cmucl sbcl) `(funcall ,expander ,gf ,mc ,methods)
  #-(or ccl clisp cmucl sbcl)
  (error "this function is not available on ~A" (lisp-implementation-type)))

(defmacro method-combination-expand (form)
  "Given a function call form, this returns the method-combination form that
   would be generated in the process of satisfying that call. This is only
   expected to work for functions whose method-combination was created using
   the long form of DEFINE-METHOD-COMBINATION."
  (let* ((gf (fdefinition (car form)))
         (mc (generic-function-method-combination gf)))
    (method-combination-expansion-form (method-combination-expander-function mc)
                                       gf
                                       mc
                                       `(compute-applicable-methods
                                         ,gf
                                         (list ,@(cdr form))))))
