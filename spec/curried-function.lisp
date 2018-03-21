(defpackage :curried-function.spec
  (:use :cl :jingoh :curried-function)
  (:import-from :curried-function #:curry)
  (:import-from :introspect-environment #:compiler-macroexpand-1)
  (:import-from :jingoh.tester #:sexp=)
  )
(in-package :curried-function.spec)
(setup :curried-function)

(requirements-about DEFCURRY)

;;;; Description:
; Define curried function.

#+syntax
(DEFCURRY name lambda-list &body body) ; => result

#?(defcurry three-plus (a b c)
    (+ a b c))
=> three-plus
,:before (fmakunbound 'three-plus)

#?(three-plus 1 2 3) => 6

#?(three-plus 1 2)
:satisfies #`(&
	       ;; Curried-function is returned, because last arg is lack.
	       (functionp $return)

	       ;; When curried-function is called with no args,
	       ;; curried-function itself is returned.
	       (functionp (funcall $return))

	       ;; When last arg is supplied, body is evaluated.
	       (= 6 (funcall $return 3))

	       ;; Example of supplying another arg.
	       (= 7 (funcall $return 4)))

#?(three-plus 1)
:satisfies #`(&
	       ;; Curried-function is returned, because rest 2 args are lack.
	       (functionp $return)

	       ;; When rest args are lack, function which awaits rest args is returned.
	       (functionp (funcall $return 2))

	       ;; When all args are supplied, the body is evaluated.
	       (= 6 (funcall (funcall $return 2)3))

	       ;; You can supply rest args at once.
	       (= 6 (funcall $return 2 3))
	       )

;;;; Arguments and Values:

; name := symbol, otherwise error.
#?(defcurry "not-symbol" ()'#:dummy)
:signals error
; Not evaluated.
#?(defcurry (intern "NOT-EVALUATED")()'#:dummy)
:signals error

; lambda-list := list which contains only symbol, otherwise signals an error.
#?(defcurry test ("not-symbol") '#:dummy)
:signals error
; This `LAMBDA-LIST` can contain only required parameters.
; I.e. when `LAMBDA-LIST-KEYWORD` is found, an error is signaled.
#?(defcurry test (&rest |<---invalid|) '#:dummy)
:signals error
; Not evaluated.
#?(defcurry test (append '(not)'(evaluated)) '#:dummy)
:signals error

; body := Implicit-progn. Evaluated.

; result := NAME
#?(defcurry this-is-returned () '#:dummy)
=> THIS-IS-RETURNED
,:after (fmakunbound 'this-is-returned)

;;;; Affected By:
; None

;;;; Side-Effects:
; Modify global environment.

;;;; Notes:
; `BODY` can contains documentation.
; When documentation string is specified, it is associated to `NAME`.
; After `DEFCURRY`, you can retrieve such string with `(documentation 'name 'function)`
#?(defcurry has-doc()
    "This!"
    '#:dummy)
=> HAS-DOC
,:before (fmakunbound 'has-doc)
#?(documentation 'has-doc 'function)
=> "This!"
,:test equal

; `BODY` can contains declaration.
#?(defcurry has-declare(a b c)
    (declare (ignore b)
	     (type fixnum a c))
    (+ a c))
=> HAS-DECLARE
,:before (fmakunbound 'has-declare)

#?(has-declare 1 2 3) => 4

#?(has-declare 1 2 "3") => unspecified ; implementation may error or warning or ...?

; You should not `DECLAIM` about `FTYPE` for `DEFCURRY`ed function.
; Instead, you can declare `API` in `DEFCURRY` form.

#?(defcurry declare-api(a b c)
    (declare(api(symbol sequence sequence)sequence))
    (concatenate a b c))
=> DECLARE-API
,:before (fmakunbound 'declare-api)
#?(declare-api 'string "Hello" '(#\!))
=> "Hello!"
,:test equal
#?(declare-api 'string "Hello" #\!) => unspecified ; implementation may error or warning or ...?

; `DEFCURRY`ed function has its own compiler-macro.
#?(defcurry compiler-macro-example(a b c)
    (+ a b c))
=> COMPILER-MACRO-EXAMPLE
,:before (fmakunbound 'compiler-macro-example)

#?(compiler-macroexpand-1 '(compiler-macro-example 1 2 3))
=> (let((a 1)
	(b 2)
	(c 3))
     (+ a b c))
,:test equal

#?(compiler-macroexpand-1 '(compiler-macro-example 1 2))
=>
(let((a 1)
     (b 2))
  (labels((curry(&optional(c nil c-p))
	    (if c-p
	      (+ a b c)
	      #'curry)))
    #'curry))
,:test sexp=

#?(compiler-macroexpand-1 '(compiler-macro-example 1))
=>
(let((a 1))
  (labels((curry(&optional(b nil b-p)(c nil c-p))
	    (if b-p
	      (if c-p
		(+ a b c)
		(labels((curry(&optional(c nil c-p))
			  (if c-p
			    (+ a b c)
			    #'curry)))
		  #'curry))
	      #'curry)))
    #'curry))
,:test sexp=

#?(compiler-macroexpand-1 '(compiler-macro-example))
=> #'compiler-macro-example
,:test equal

;;;; Exceptional-Situations:

(requirements-about \\)

;;;; Description:
; Make anonymous curried function.

#+syntax
(\\ lambda-list &body body) ; => result

#?(\\(a b c)(+ a b c))
:satisfies #`(& (functionp $return)
		(functionp (funcall $return))
		(functionp (funcall $return 1))
		(functionp (funcall (funcall $return 1)2))
		(functionp (funcall $return 1 2))
		(functionp (funcall (funcall $return 1 2)))
		(= 6 (funcall (funcall (funcall $return 1)2)3))
		(= 6 (funcall $return 1 2 3))
		(= 6 (funcall (funcall $return)1 2 3))
		)

;;;; Arguments and Values:

; lambda-list := list which contains only symbol, otherwise signals an error.
#?(\\ ("not-symbol") '#:dummy)
:signals error
; This `LAMBDA-LIST` can contain only required parameters.
; I.e. when `LAMBDA-LIST-KEYWORD` is found, an error is signaled.
#?(\\ (&rest |<---invalid|) '#:dummy)
:signals error
; Not evaluated.
#?(\\ (append '(not)'(evaluated)) '#:dummy)
:signals error

; body := Implicit-progn, evaluated.

; result := Curried function.

;;;; Affected By:
; None

;;;; Side-Effects:
; None

;;;; Notes:

;;;; Exceptional-Situations:


(requirements-about SECTION)

;;;; Description:

#+syntax
(SECTION op &rest args) ; => result

;;;; Arguments and Values:

; op := Function-name, otherwise error.
#?(section #'append '(not) _ '(function-name))
:signals error

; args := T
; When argument is symbol named "_", return curried function which awaits such position's actual argument.
#?(section concatenate 'string _ " world")
:satisfies #`(& (functionp $return)
		(string= "Hello world"
			 (funcall $return "Hello")))
; Multiple _ is valid.
; In such cases, arguments are supplied by left to right order.
#?(section concatenate _ _ " world")
:satisfies #`(& (functionp $return)
		(functionp (funcall $return 'string))
		(string= "Hello world"
			 (funcall (funcall $return 'string)
				  "Hello"))
		(string= "Good-bye world"
			 (funcall $return 'string "Good-bye")))

; result := (or function T)
; If there is no underscore, the body is evaluated.
#?(section concatenate 'string "Hello" " world")
=> "Hello world"
,:test string=
#?(section concatenate 'string "Hello")
=> "Hello"
,:test string=
#?(section concatenate 'string "Hello " _)
:satisfies #`(& (functionp $return)
		(string= "Hello world"
			 (funcall $return "world")))
; Otherwise curried function is returned.
#?(section concatenate _ "Hello" " world")
:be-the function

;;;; Affected By:
; None

;;;; Side-Effects:
; None

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about CURRIED-LABELS)

;;;; Description:
; Local curried function.

#+syntax
(CURRIED-LABELS (label*) &body body) ; => result

#?(curried-labels((3plus(a b c)
		   (+ a b c)))
    (3plus 1 2 3))
=> 6

#?(curried-labels((3plus(a b c)
		   (+ a b c)))
    (3plus 1 2))
:satisfies #`(& (functionp $result)
		(= 6 (funcall $result 3)))

#?(curried-labels((3plus(a b c)
		   (+ a b c)))
    (maplist (lambda(list)
	       (apply #'3plus list))
	     '(3 2 1)))
:satisfies #`(destructuring-bind(first second third)$result
	       (& 
		 ; first gets all argumemts.
		 (= 6 first)
		 
		 ; second lacks last argument, so function is returned.
		 (functionp second)
		 ; such function evaluates body when last argument is applied.
		 (= 6 (funcall second 3))

		 ; third get only first argument, so function is returned.
		 (functionp third)
		 ; such function awaits rest args.
		 (functionp (funcall third 2))
		 (= 6 (funcall (funcall third 2)
			       3))
		 (= 6 (funcall third 2 3))))
;;;; Arguments and Values:

; label := (name lambda-list &body body)
#?(curried-labels()
    :valid)
=> :VALID

; name := (and symbol (not boolean))
#?(curried-labels(("not-symbol"():dummy))
    :dummy)
:signals error
#?(curried-labels((t():<---invalid))
    :dummy)
:signals error
; Not evaluated.
#?(curried-labels(((intern "Not evaluated")():dummy))
    :dummy)
:signals error

; lambda-list := list which contains only symbol, otherwise signals an error.
#?(curried-labels((test("not-symbol"):dummy))
    (test :dummy))
:signals error
; This `LAMBDA-LIST` can contain only required parameters.
; I.e. when `LAMBDA-LIST-KEYWORD` is found, an error is signaled.
#?(curried-labels((test(&rest |<---invalid|)
		    '#:dummy))
    (test))
:signals error
; Not evaluated.
#?(curried-labels((test(append '(not)'(evaluated))
		    '#:dummy))
    (test))
:signals error

; body := implicit progn, evaluated.

; result := T

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; BODY can contain documentation.
; But it will be discarded.
#?(curried-labels((has-doc(a b c)
		    "This documentation is discarded."
		    (+ a b c)))
    (has-doc 1 2 3))
=> 6

; BODY can contain declaration.
#?(curried-labels((with-declare(a b c)
		    (declare (ignore b)
			     (type fixnum a c))
		    (+ a c)))
    (with-declare 1 2 3))
=> 4
;;;; Exceptional-Situations:


