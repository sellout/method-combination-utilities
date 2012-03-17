Method combinations are one of the more obscure bits of Common Lisp. I think they're pretty fantastic and should be understood and used by more developers. This library is an attempt to provide some tools to aid in defining new method combinations as well as a few simple combinations that may be generally useful.

## (METHOD-COMBINATION-EXPAND _form_)

This macro is to method combinations what MACROEXPAND is to macros. Given a function call form, it'll expand to the form used to call the methods.

For example, given:

```common-lisp
(defgeneric print-slots (object)
  (:method-combination basic progn t)
  (:method progn ((object superclass)) ...)
  (:method progn ((object subclass)) ...))

(pprint (method-combination-expand (print-slots subclass-instance)))
```

the result should look something like:

```common-lisp
(PROGN (CALL-METHOD #<STANDARD-METHOD PRINT-SLOTS PROGN (SUBCLASS)>)
       (CALL-METHOD #<STANDARD-METHOD PRINT-SLOTS PROGN (SUPERCLASS)>))
```

This can be extremely helpful both for users of method combinations and developers of them.

## Definition Helpers

### (CALL-METHODS _methods_)

This is `FLET`ed (or expanded in-line) in almost all method combinations, including [every `DEFINE-METHOD-COMBINATION` example in the spec](http://www.lispworks.com/documentation/lw50/CLHS/Body/m_defi_4.htm). The name isn't the best, but it has a strong tradition.

This function just returns a list of `CALL-METHOD` forms, one for each method passed in. EG:

```common-lisp
(call-methods '(1 2 3))
=> ((CALL-METHOD 1) (CALL-METHOD 2) (CALL-METHOD 3))
```

### (COMBINE-STANDARD-METHODS _primary-methods_ &optional _around-methods_ _before-methods_ _after-methods_)

In a lot of custom method combinations there is some attempt to keep the behavior of the `STANDARD` method combination. This function manages that portion of the method combination so other components can be layered on top.

This example converts the 55-line WRAPPING-STANDARD method combination from [arnesi](http://common-lisp.net/project/bese/arnesi.html) into a much cleaner 17-line version.

```common-lisp
(define-method-combination wrapping-standard
    (&key (wrap-around-order :most-specific-last)
          (around-order :most-specific-first)
          (before-order :most-specific-first)
          (wrapping-order :most-specific-last)
          (primary-order :most-specific-first)
          (after-order :most-specific-last))
  ((wrap-around (:wrap-around) :order wrap-around-order)
   (around (:around) :order around-order)
   (before (:before) :order before-order)
   (wrapping (:wrapping) :order wrapping-order)
   (primary () :order primary-order :required t)
   (after (:after) :order after-order))
  "Same semantics as standard method combination but allows \"wrapping\"
   methods. Ordering of methods:

  (wrap-around
    (around
      (before)
      (wrapping
        (primary))
      (after)))

  :wrap-around and :wrapping methods can use call-next-method."
  ;; :WRAP-AROUND is similar to :AROUND and :WRAPPING is similar to primary, so
  ;; each pair can be concatenated and then we can just apply the standard
  ;; combination.
  (combine-standard-methods (append wrapping primary)
                            (append wrap-around around)
                            before
                            after))
```

### (WRAP-PRIMARY-FORM _primary-form_ &optional _around-methods_ _before-methods_ _after-methods_)

This is similar to `COMBINE-STANDARD-METHODS`, but it takes an already-computed primary form, rather than a list of primary methods. This is because it's fairly common to have some custom behavior for the primary methods and then combine it with the usual `:AROUND`/`:BEFORE`/`:AFTER` methods.

This example is simplified from pretty much the entire [nisp-standard-combination.lisp](https://github.com/nixeagle/nisp/blob/master/old-util/nisp-standard-combination.lisp) file from the [nisp](https://github.com/nixeagle/nisp) project. Note that (based on the pathname), this combination (or at least the version I linked to) is probably obsolete.

```common-lisp
(define-method-combination nisp-standard (&key hook)
  ((defaulting (:defaulting) :order :most-specific-last)
   (meta-around (:meta-around))
   (around (:around))
   (before (:before))
   (primary () :required t)
   (after (:after) :order :most-specific-last))
  "This behaves similarly to `STANDARD`, but wraps the whole thing with
  :DEFAULTING (most-recent-last) and :META-AROUND methods. Also, if a HOOK is
   passed, the primary methods are treated as in the BASIC combination, with
   HOOK as the operator.

  (defaulting
    (meta-around
      (around
        (before)
        (primary)
        (after))))"
  (wrap-primary-form (if hook
                         `(,hook ,@(call-methods primary))
                         `(call-method ,(first primary) ,(rest primary)))
                     (append defaulting meta-around around)
                     before
                     after))
```

## Method Combinations

### (PRIMARY)

The `PRIMARY` method combination is a stripped-down version of the `STANDARD` method combination that _only_ allows primary methods. Taken from [ISLISP](http://www.islisp.info/)’s `NIL` method combination (but renamed because CLs with package locks don't like it) after it was suggested by [Pascal Costanza](http://www.p-cos.net/).

### (LAX)

The `LAX` method combination is intended for use in cases where you are handling your qualifiers in a custom method class and don't _really_ need a custom method combination, but you need the `STANDARD` method combination to quietly ignore your special qualifiers. (a la Closer’s [Filtered Functions](http://common-lisp.net/project/closer/filtered.html))

### (BASIC _operator_ &optional _identity-with-one-argument-p_ _order_)

The intent of the `BASIC` method combination is to obviate the need for the short form of `DEFINE-METHOD-COMBINATION`. With this method combination, you have the same functionality but without having to define the method combination separately from where it's used.

Of course, I've never actually seen any code that uses the short form of `DEFINE-METHOD-COMBINATION` (and I've looked). However, since all the (non-`STANDARD`) built-in method combinations behave as if they were created using the short form, this also acts as a proposal for any updates to Common Lisp – rather than having ten built-in combinations (four of which I have never seen used), have only two: `STANDARD` and `BASIC`, and eliminate the short form of `DEFINE-METHOD-COMBINATION`.

For example, the built-in `+` combination can be replicated with `(basic + t)`.

### (APPEND/NCONC &optional _order_)

Using either the built-in `APPEND` or `NCONC` combinations (or those functions as the operator for the `BASIC` combination) misses something important. `NCONC` is largely a non-consing optimization of `APPEND` (if you are somehow using the method combination to make circular structures or something, then the built-in `NCONC` is probably a better bet). If you use the built-in combinations, then every method on your function needs to share the same qualifier, say, `APPEND`. If you later decide that it is safe to use `NCONC`, then you need to change the combination on the generic function and change _all_ of your methods – and hope that no users of your library have added additional `APPEND` methods to the function.

The `APPEND/NCONC` combination, however, allows the use of either qualifier. Methods that return a list safe to be `RPLACD`ed can use the `NCONC` qualifier, while those that don't can use `APPEND`. If all but the last applicable primary method (most- or least-specific, depending on the `ORDER` parameter) use the `NCONC` qualifier, than the results will be concatenated with `NCONC`, otherwise it will use `APPEND`.

This allows the optimization to be added on a case-by-case basis and the gains to be had where possible, without tying users to your specific implementation decision.
