(defpackage :curried-function
  (:use :cl)
  (:export
    #:defcurry
    #:\\
    #:curried-labels
    #:api
    #:section
    #:syntax	; for readtable name.
    ))
(in-package :curried-function)

;;;; API DECLARATION
(declaim(declaration api))

;;;; DEFCURRY
(defmacro defcurry(name lambda-list &body body)
  ;; Trivial syntax check.
  (check-type name (and symbol (not (or keyword boolean))))
  (assert (every #'symbolp lambda-list))
  (assert (notany (lambda(elt)
		    (find elt lambda-list-keywords))
		  lambda-list))
  ;; binds
  (multiple-value-bind(BODY declarations DOCUMENTATION)(alexandria:parse-body body :documentation t)
    (multiple-value-bind(API other-declares IGNORES)(parse-declarations declarations)
      (setf body ; as canonicalize
	    (append (when other-declares
		      `((DECLARE ,@other-declares)))
		    `((BLOCK ,name ,@body))))
      (let((OPTIONAL-LAMBDA-LIST(optional-lambda-list lambda-list)))
	;; body
	`(EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
	   ,@(when api
	       `((DECLAIM,(apply #'<Ftype-Form> name (cdr api)))))
	   ,(<Compiler-Macro-Form> name lambda-list body api optional-lambda-list ignores)
	   (SETF (SYMBOL-FUNCTION 
		   ;; When `DEFCURRY`ed function is recursive one,
		   ;; Compiler claims `UNDEFINED-FUNCTION`.
		   ;; To avoid that, we `DEFUN` dummy one.
		   (DEFUN,name,(when lambda-list
				 `(&OPTIONAL ,@optional-lambda-list))
		     ,@(when lambda-list
			 `((DECLARE(IGNORE ,@(loop :for elt :in optional-lambda-list
						   :collect (car elt)
						   :collect (caddr elt))))))))
		 ,(if lambda-list
		    (<Curry-form> body optional-lambda-list (cadr api) (caddr api)ignores)
		    `(LAMBDA(),@body)))
	   ,@(when documentation
	       `((SETF (DOCUMENTATION ',name 'FUNCTION),documentation)))
	   ',name)))))

;; Disadvantage of curried-function is runtime dispatching.
;; How many initial supplied arguments are known in compile time.
;; We reduce run time dispatching as possible as we can.
(defun <Compiler-Macro-Form>(name lambda-list body api optional-lambda-list ignores)
  `(DEFINE-COMPILER-MACRO,name(&WHOLE WHOLE &REST ARGS)
     (ASSERT(<= (LENGTH ARGS),(length lambda-list))()
       "Too much args ~S" WHOLE)
     ,(if(null lambda-list)
	(if(cdr body)
	  `',(cons 'LOCALLY body)
	  `',(car body))
	`(LET((LENGTH(LENGTH ARGS)))
	   (IF(ZEROP LENGTH)
	     '#',name
	     `(LET,(MAPCAR #'LIST ',lambda-list ARGS)
		,@,@(when ignores
		      `((<ignore-declaration> (SUBSEQ ',optional-lambda-list 0 (LENGTH ARGS))
					      ',ignores)))
		,,@(when api
		     (mapcar (lambda(type var)
			       `'(DECLARE(TYPE ,type ,var)))
			     (cadr api) lambda-list))
		,(LET((TYPES(NTHCDR LENGTH ',(cadr api)))
		      (OPTIONAL-LAMBDA-LIST(NTHCDR LENGTH ',optional-lambda-list)))
		   (<CURRY-FORM> ',body OPTIONAL-LAMBDA-LIST TYPES
				 ',(caddr api)',ignores))))))))

(defun parse-declarations(decls)
  (flet((INTEGRATE(k-vs)
	  (loop :for (nil . rest) :in k-vs
		:append rest))
	)
    (loop :for option :in (INTEGRATE decls)
	  :when (eq 'api (car option))
	  :collect option :into apis
	  :else :if (eq 'ignore (car option))
	  :collect option :into ignores
	  :else :collect option :into decls
	  :finally (return
		     (if(cdr apis)
		       (progn (warn "Multiple API declaration is invalid.~%Ignored ~S."
				    (cdr apis))
			      (values (car apis)decls (INTEGRATE ignores)))
		       (values (car apis)decls (INTEGRATE ignores)))))))

(defun <Ftype-Form>(name types return-type)
  `(FTYPE (FUNCTION ,(when types
		       `(&OPTIONAL ,@(loop :for type :in types
					   :collect `(OR NULL ,type))))
		    ,(if types
		       `(VALUES (OR FUNCTION,return-type))
		       return-type))
	  ,name))

#+Example-of-what-<Curry-Form>-makes.
(labels((curry(&optional (a nil a-p)(b nil b-p)(c nil c-p)) ; <--- entry-point
	  (if a-p
	    (if b-p ; <--- rec
	      (if c-p ; <--- rec
		(+ a b c) ; <--- body form
		(labels((curry (&optional (c nil c-p))) ; <--- entry-point
			(if c-p ; <--- rec
			  (+ a b c) ; <--- body form
			  #'curry))
		  #'curry))
	      (labels((curry(&optional (b nil b-p)(c nil c-p)) ; <--- entry-point
			(if b-p
			  (if c-p ; <--- rec
			    (+ a b c) ; <--- body form
			    (labels((curry(&optional(c  nil c-p)) ; <--- entry-point
				      (if c-p ; <--- rec
					(+ a b c) ; <--- body form
					#'curry)))
			      #'curry))
			  #'curry)))
		#'curry))
	    #'curry)))
  #'curry)

(defun <Curry-Form> (body optional-lambda-list &optional types return-type ignores name)
  (let((curry (or name (gensym "CURRY"))))
    (labels((ENTRY-POINT(list types)
	      (if(endp list)
		(<BODY-FORM> body)
		`(LABELS((,curry(&OPTIONAL ,@list)
			   ,@(<Ignore-declaration> list ignores)
			   (IF ,(caddar list)
			     ,(rec (cdr list)(cdr types))
			     #',curry)))
		   #',curry)))
	    (REC(list types)
	      (if(endp list)
		(<body-form> body)
		`(IF,(caddar list)
		   ,(<ARG-SUPPLIED-FORM> types list
					 (lambda()(REC (cdr list)
						       (cdr types))))
		   ,(ENTRY-POINT list types))))
	    (<ARG-SUPPLIED-FORM>(types list cont)
	      (if types
		(WRAP-WITH-DECLARE (caar list)
				   (car types)
				   (funcall cont))
		(funcall cont)))
	    (WRAP-WITH-DECLARE(var type body)
	      `(LOCALLY
		 (DECLARE(TYPE ,type ,var))
		 ,body))
	    (<BODY-FORM>(body)
	      (if return-type
		`(FLET((#0=#:BODY()
			,@body))
		   (DECLARE(FTYPE(FUNCTION(),return-type)#0#))
		   (#0#))
		(if(cdr body)
		  `(LOCALLY ,@body)
		  (car body))))
	    )
      (ENTRY-POINT optional-lambda-list types))))

(defun <Ignore-Declaration>(optional-lambda-list ignores)
  (loop :for (var) :in optional-lambda-list
	:when (find var ignores)
	:collect var :into i
	:finally (when i
		   (return `((DECLARE(IGNORE ,@i)))))))

(defun optional-lambda-list(lambda-list)
  (mapcar (lambda(x)
	    `(,x NIL ,(gensym (format nil "~A-P"x))))
	  lambda-list))

;;;; ANONYMOUS CURRIED FUNCTION
(defmacro \\(lambda-list &body body)
  ;; Trivial syntax check.
  (assert (every #'symbolp lambda-list))
  (assert (notany (lambda(elt)(find elt lambda-list-keywords))lambda-list))
  ;; binds
  (multiple-value-bind(body declarations documentation)(alexandria:parse-body body :documentation t)
    (declare(ignore documentation))
    (multiple-value-bind(api other-declares ignores)(parse-declarations declarations)
      (setf body (append (when other-declares
			   `((DECLARE ,@other-declares)))
			 body))
      (let((optional-lambda-list(optional-lambda-list lambda-list)))
	;; body
	(<Curry-Form> body optional-lambda-list (cadr api)(caddr api)ignores)))))

;;;; SECTION
(defmacro section (op &rest args)
  ;; Trivial syntax check.
  (check-type op (or symbol (cons (eql lambda)T)))
  (when(typep op '(cons (eql lambda)t))
    (assert (every #'symbolp (cadr op)))
    (assert (notany (lambda(elt)
		      (find elt lambda-list-keywords))
		    (cadr op))))
  ;; body
  (if(null args)
    `#',op
    (<Section-Form> op args)))

(defun <Section-Form>(op args)
  (let*((gensyms(gensyms(count-if #'underscorep args)))
	(optional-lambda-list(optional-lambda-list gensyms)))
    (if gensyms
      (<Curry-Form> (<Section-Body-Form> op args gensyms) optional-lambda-list)
      `(,op ,@args))))

(defun underscorep (thing)
  (and (symbolp thing)
       (string= "_" thing)))

(defun gensyms(num)
  (loop :repeat num :collect (gensym)))

(defun <Section-Body-Form>(op args gensyms)
  (labels((rec(args gensyms &optional acc)
	    (if(endp args)
	      (nreverse acc)
	      (body(car args)(cdr args)gensyms acc)))
	  (body(arg rest gensyms acc)
	    (if(underscorep arg)
	      (rec rest (cdr gensyms)(push (car gensyms)acc))
	      (rec rest gensyms (push arg acc)))))
    `((,op ,@(rec args gensyms)))))

;;;; CURRIED-LABELS
(defmacro curried-labels(labels &body body)
  `(LABELS,(mapcar #'<Label-form> labels)
     ,@body))

(defun <Label-form> (definition)
  ;; binds
  (destructuring-bind(NAME lambda-list &body body)definition
    ;; Trivial syntax check.
    (check-type name (and symbol (not (or keyword boolean))))
    (assert (every #'symbolp lambda-list))
    (assert (notany (lambda(elt)
		      (find elt lambda-list-keywords))
		    lambda-list))
    (multiple-value-bind(BODY declarations)(alexandria:parse-body body :documentation t)
      (multiple-value-bind(API other-declares IGNORES)(parse-declarations declarations)
	(setf body ; as canonicalize
	      (append (when other-declares
			`((DECLARE ,@other-declares)))
		      `((BLOCK ,name ,@body))))
	(let((OPTIONAL-LAMBDA-LIST(optional-lambda-list lambda-list)))
	  ;; body
	  (caadr(<Curry-Form> body optional-lambda-list (cadr api)(caddr api)ignores name)))))))

;;;; READ-TABLE
(defun |#`-reader|(stream char number)
  (declare(ignore char number))
  `(section ,@(read stream t t t)))

(named-readtables:defreadtable syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\` '|#`-reader|))
