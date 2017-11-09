; vim: ft=lisp et
(in-package :asdf)
(defsystem :curried-function.test
  :depends-on
  (:jingoh "curried-function" "introspect-environment")
  :components
  ((:file "curried-function"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :curried-function)))
