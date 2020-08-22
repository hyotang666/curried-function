(defpackage :curried-function
  (:use :cl)
  (:export #:defcurry
           #:|\\|
           #:curried-labels
           #:api
           #:section
           #:syntax ; for readtable name.
           ))

(in-package :curried-function)

;;;; API DECLARATION

(declaim (declaration api))

;;;; DEFCURRY

(defmacro defcurry (name lambda-list &body body)
  ;; Trivial syntax check.
  (check-type name (and symbol (not (or keyword boolean))))
  (assert (every #'symbolp lambda-list))
  (assert (notany (lambda (elt) (find elt lambda-list-keywords)) lambda-list))
  ;; binds
  (multiple-value-bind (body declarations documentation)
      (alexandria:parse-body body :documentation t)
    (multiple-value-bind (api other-declares ignores)
        (parse-declarations declarations)
      (setf body ; as canonicalize
            (append
              (when other-declares
                `((declare ,@other-declares)))
              `((block ,name ,@body))))
      (let ((optional-lambda-list (optional-lambda-list lambda-list)))
        ;; body
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           ,@(when api
               `((declaim ,(apply #'<ftype-form> name (cdr api)))))
           ,(<compiler-macro-form> name lambda-list body api
                                   optional-lambda-list ignores)
           (setf (symbol-function ;; When `DEFCURRY`ed function is recursive one,
                                  ;; Compiler claims `UNDEFINED-FUNCTION`.
                                  ;; To avoid that, we `DEFUN` dummy one.
                                  (defun ,name
                                         ,(when lambda-list
                                            `(&optional
                                              ,@optional-lambda-list))
                                    ,@(when lambda-list
                                        `((declare
                                           (ignore
                                            ,@(loop :for elt
                                                         :in optional-lambda-list
                                                    :collect (car elt)
                                                    :collect (caddr elt))))))))
                   ,(if lambda-list
                        (<curry-form> body optional-lambda-list (cadr api)
                                      (caddr api) ignores)
                        `(lambda () ,@body)))
           ,@(when documentation
               `((setf (documentation ',name 'function) ,documentation)))
           ',name)))))

;; Disadvantage of curried-function is runtime dispatching.
;; How many initial supplied arguments are known in compile time.
;; We reduce run time dispatching as possible as we can.

(defun <compiler-macro-form>
       (name lambda-list body api optional-lambda-list ignores)
  `(define-compiler-macro ,name (&whole whole &rest args)
     (assert (<= (length args) ,(length lambda-list)) ()
       "Too much args ~S" whole)
     ,(if (null lambda-list)
          (if (cdr body)
              `',(cons 'locally body)
              `',(car body))
          `(let ((length (length args)))
             (if (zerop length)
                 '#',name
                 `(let ,(mapcar #'list ',lambda-list args)
                    ,@,@(when ignores
                          `((<ignore-declaration>
                              (subseq ',optional-lambda-list 0 (length args))
                              ',ignores)))
                    ,,@(when api
                         (mapcar
                           (lambda (type var) `'(declare (type ,type ,var)))
                           (cadr api) lambda-list))
                    ,(let ((types (nthcdr length ',(cadr api)))
                           (optional-lambda-list
                            (nthcdr length ',optional-lambda-list)))
                       (<curry-form> ',body optional-lambda-list types
                                     ',(caddr api) ',ignores))))))))

(defun parse-declarations (decls)
  (flet ((integrate (k-vs)
           (loop :for (nil . rest) :in k-vs
                 :append rest)))
    (loop :for option :in (integrate decls)
          :when (eq 'api (car option))
            :collect option :into apis
          :else :if (eq 'ignore (car option))
            :collect option :into ignores
          :else
            :collect option :into decls
          :finally (return
                    (if (cdr apis)
                        (progn
                         (warn
                           "Multiple API declaration is invalid.~%Ignored ~S."
                           (cdr apis))
                         (values (car apis) decls (integrate ignores)))
                        (values (car apis) decls (integrate ignores)))))))

(defun <ftype-form> (name types return-type)
  `(ftype (function
           ,(when types
              `(&optional
                ,@(loop :for type :in types
                        :collect `(or null ,type))))
           ,(if types
                `(values (or function ,return-type))
                return-type))
          ,name))

#+example-of-what-<curry-form>-makes.
(labels ((curry (&optional (a nil a-p) (b nil b-p) (c nil c-p)) ; <---
                                                                ; entry-point
           (if a-p
               (if b-p ; <--- rec
                   (if c-p ; <--- rec
                       (+ a b c) ; <--- body form
                       (labels ((curry (&optional (c nil c-p)) ; <---
                                                               ; entry-point
                                  (if c-p ; <--- rec
                                      (+ a b c) ; <--- body form
                                      #'curry)))
                         #'curry))
                   (labels ((curry (&optional (b nil b-p) (c nil c-p)) ; <---
                                                                       ; entry-point
                              (if b-p
                                  (if c-p ; <--- rec
                                      (+ a b c) ; <--- body form
                                      (labels ((curry (&optional (c nil c-p)) ; <---
                                                                              ; entry-point
                                                 (if c-p ; <--- rec
                                                     (+ a b c) ; <--- body form
                                                     #'curry)))
                                        #'curry))
                                  #'curry)))
                     #'curry))
               #'curry)))
  #'curry)

(defun <curry-form>
       (body optional-lambda-list &optional types return-type ignores name)
  (let ((curry (or name (gensym "CURRY"))))
    (labels ((entry-point (list types)
               (if (endp list)
                   (<body-form> body)
                   `(labels ((,curry (&optional ,@list)
                               ,@(<ignore-declaration> list ignores)
                               (if ,(caddar list)
                                   ,(rec (cdr list) (cdr types))
                                   #',curry)))
                      #',curry)))
             (rec (list types)
               (if (endp list)
                   (<body-form> body)
                   `(if ,(caddar list)
                        ,(<arg-supplied-form> types list
                                              (lambda ()
                                                (rec (cdr list) (cdr types))))
                        ,(entry-point list types))))
             (<arg-supplied-form> (types list cont)
               (if types
                   (wrap-with-declare (caar list) (car types) (funcall cont))
                   (funcall cont)))
             (wrap-with-declare (var type body)
               `(locally (declare (type ,type ,var)) ,body))
             (<body-form> (body)
               (if return-type
                   `(flet ((#0=#:body ()
                             ,@body))
                      (declare (ftype (function nil ,return-type) #0#))
                      (#0#))
                   (if (cdr body)
                       `(locally ,@body)
                       (car body)))))
      (entry-point optional-lambda-list types))))

(defun <ignore-declaration> (optional-lambda-list ignores)
  (loop :for (var) :in optional-lambda-list
        :when (find var ignores)
          :collect var :into i
        :finally (when i
                   (return `((declare (ignore ,@i)))))))

(defun optional-lambda-list (lambda-list)
  (mapcar (lambda (x) `(,x nil ,(gensym (format nil "~A-P" x)))) lambda-list))

;;;; ANONYMOUS CURRIED FUNCTION

(defmacro |\\| (lambda-list &body body)
  ;; Trivial syntax check.
  (assert (every #'symbolp lambda-list))
  (assert (notany (lambda (elt) (find elt lambda-list-keywords)) lambda-list))
  ;; binds
  (multiple-value-bind (body declarations documentation)
      (alexandria:parse-body body :documentation t)
    (declare (ignore documentation))
    (multiple-value-bind (api other-declares ignores)
        (parse-declarations declarations)
      (setf body
              (append
                (when other-declares
                  `((declare ,@other-declares)))
                body))
      (let ((optional-lambda-list (optional-lambda-list lambda-list)))
        ;; body
        (<curry-form> body optional-lambda-list (cadr api) (caddr api)
                      ignores)))))

;;;; SECTION

(defmacro section (op &rest args)
  ;; Trivial syntax check.
  (check-type op (or symbol (cons (eql lambda) t)))
  (when (typep op '(cons (eql lambda) t))
    (assert (every #'symbolp (cadr op)))
    (assert (notany (lambda (elt) (find elt lambda-list-keywords)) (cadr op))))
  ;; body
  (if (null args)
      `#',op
      (<section-form> op args)))

(defun <section-form> (op args)
  (let* ((gensyms (gensyms (count-if #'underscorep args)))
         (optional-lambda-list (optional-lambda-list gensyms)))
    (if gensyms
        (<curry-form> (<section-body-form> op args gensyms)
                      optional-lambda-list)
        `(,op ,@args))))

(defun underscorep (thing) (and (symbolp thing) (string= "_" thing)))

(defun gensyms (num)
  (loop :repeat num
        :collect (gensym)))

(defun <section-body-form> (op args gensyms)
  (labels ((rec (args gensyms &optional acc)
             (if (endp args)
                 (nreverse acc)
                 (body (car args) (cdr args) gensyms acc)))
           (body (arg rest gensyms acc)
             (if (underscorep arg)
                 (rec rest (cdr gensyms) (push (car gensyms) acc))
                 (rec rest gensyms (push arg acc)))))
    `((,op ,@(rec args gensyms)))))

;;;; CURRIED-LABELS

(defmacro curried-labels (labels &body body)
  `(labels ,(mapcar #'<label-form> labels)
     ,@body))

(defun <label-form> (definition)
  ;; binds
  (destructuring-bind
      (name lambda-list &body body)
      definition
    ;; Trivial syntax check.
    (check-type name (and symbol (not (or keyword boolean))))
    (assert (every #'symbolp lambda-list))
    (assert (notany (lambda (elt) (find elt lambda-list-keywords))
                    lambda-list))
    (multiple-value-bind (body declarations)
        (alexandria:parse-body body :documentation t)
      (multiple-value-bind (api other-declares ignores)
          (parse-declarations declarations)
        (setf body ; as canonicalize
              (append
                (when other-declares
                  `((declare ,@other-declares)))
                `((block ,name ,@body))))
        (let ((optional-lambda-list (optional-lambda-list lambda-list)))
          ;; body
          (caadr
            (<curry-form> body optional-lambda-list (cadr api) (caddr api)
                          ignores name)))))))

;;;; READ-TABLE

(defun |#`-reader| (stream char number)
  (declare (ignore char number))
  `(section ,@(read stream t t t)))

(named-readtables:defreadtable syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\` '|#`-reader|))

;;;; PRETTY-PRINTER

(set-pprint-dispatch '(cons (member defcurry)) (pprint-dispatch '(defun)))

(set-pprint-dispatch '(cons (member curried-labels))
                     (pprint-dispatch '(labels)))
