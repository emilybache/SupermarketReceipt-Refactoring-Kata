;;;; package.lisp

(defpackage :supermarket-receipt/tests
  (:use :common-lisp
	      :supermarket-receipt)
  (:import-from :parachute
   :define-test
   :is)
  (:import-from :cl-mock
   :with-mocks
   :answer
   :call-previous
   :invocations))
