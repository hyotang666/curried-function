; vim: ft=lisp et
(in-package :asdf)
(defsystem "curried-function"
  :depends-on
  ("alexandria")
  :pathname
  "src/"
  :components
  ((:file "curried-function")))

;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "curried-function"))))
  (handler-bind((recursive-operate #'muffle-warning))
    (test-system :curried-function.test)))
