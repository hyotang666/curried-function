; vim: ft=lisp et
(in-package :asdf)
(defsystem "curried-function"
  :depends-on
  (
   "named-readtables"   ; readtable.
   "alexandria" ; public domain utilities.
   )
  :pathname
  "src/"
  :components
  ((:file "curried-function")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "curried-function"))))
  (append (call-next-method) '((test-op "curried-function.test"))))
