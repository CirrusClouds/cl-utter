;;;; package.lisp

(defpackage #:cl-unittest
  (:use #:cl)
  (:export :test-all
   :make-log
           :*log-level*
   :deftest
           :*all-tests*))
