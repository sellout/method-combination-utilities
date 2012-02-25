Method combinations are one of the more obscure bits of Common Lisp. I think they're pretty fantastic and should be understood and used by more developers. This library is an attempt to provide some tools to aid in defining new method combinations as well as a few simple combinations that may be generally useful.

## (METHOD-COMBINATION-EXPAND _form_)

This macro is to method combinations what MACROEXPAND is to macros. Given a function call form, it'll expand to the form used to call the methods.

**NOTE**: This currently only works for CCL, but I imagine it will be fairly easy to port.

For example, given:
```common-lisp
(defgeneric print-slots (object)
  (:method-combination basic progn t)
  (:method progn ((object superclass)) …)
  (:method progn ((object subclass)) …))

(pprint (method-combination-expand (print-slots subclass-instance)))
```
the result should look something like:
```common-lisp
(PROGN (CALL-METHOD #<STANDARD-METHOD PRINT-SLOTS PROGN (SUBCLASS)>)
       (CALL-METHOD #<STANDARD-METHOD PRINT-SLOTS PROGN (SUPERCLASS)>))
```
This can be extremely helpful both for users of method combinations and developers of them.

## (COMBINE-STANDARD-METHODS _around_ _before_ _primary_ _after_)

In a lot of custom method combinations there is some attempt to keep the behavior of the `STANDARD` method combination. This function manages that portion of the method combination so other components can be layered on top.

This example converts the 55-line WRAPPING-STANDARD method combination from [arnesi](http://common-lisp.net/project/bese/arnesi.html) into a much cleaner 17-line version.

```common-lisp
;; :WRAP-AROUND is similar to :AROUND and :WRAPPING is similar to primary, so
;; each pair can be concatenated and then we can just apply the standard
;; combination.
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
  "Same semantics as standard method combination but allows
\"wrapping\" methods. Ordering of methods:

 (wrap-around
   (around
     (before)
     (wrapping
       (primary))
     (after)))

:warp-around, :around, :wrapping and :primary methods call the
next least/most specific method via call-next-method (as in
standard method combination).

The various WHATEVER-order keyword arguments set the order in
which the methods are called and be set to either
:most-specific-last or :most-specific-first."
  (combine-standard-methods (append wrap-around around)
                            before
                            (append wrapping primary)
                            after))
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
  (combine-standard-methods (append wrap-around around)
                            before
                            (append wrapping primary)
                            after))
```

## Method Combinations

### (BASIC _operator_ _identity-with-one-argument-p_ &optional _order_)

The intent of the `BASIC` method combination is to obviate the need for the short form of `DEFINE-METHOD-COMBINATION`. With this method combination, you have the same functionality but without having to define the method combination separately from where it's used.

Of course, I've never actually seen any code that uses the short form of `DEFINE-METHOD-COMBINATION` (and I've looked). However, since all the (non-`STANDARD`) built-in method combinations behave as if they were created using the short form, this also acts as a proposal for any updates to Common Lisp – rather than having ten built-in combinations (four of which I have never seen used), have only two: `STANDARD` and `BASIC`, and eliminate the short form of `DEFINE-METHOD-COMBINATION`.

### (APPEND/NCONC &optional _order_)

Using either the built-in `APPEND` or `NCONC` combinations (or those functions as the operator for the `BASIC` combination) misses something important. `NCONC` is largely a non-consing optimization of `APPEND` (if you are somehow using the method combination to make circular structures or something, then the built-in `NCONC` is probably a better bet). If you use the built-in combinations, then every method on your function needs to share the same qualifier, say, `APPEND`. If you later decide that it is safe to use `NCONC`, then you need to change the combination and change all of your methods – and hope that no users have added additional `APPEND` methods to the function.

The `APPEND/NCONC` combination, however, allows the use of either qualifier. Methods that return a list safe to be `RPLACD`ed can use the `NCONC` qualifier, while those that don't can use `APPEND`. If all but the last applicable method (most- or least-specific, depending on the `ORDER` parameter) use the `NCONC` qualifier, than the results will be concatenated with `NCONC`, otherwise it will use `APPEND`.

This allows the optimization to be added on a case-by-case basis and the gains to be had where possible, without tying users to your specific implementation decision.