(in-package #:common-lisp-user)

(defpackage #:tazivor
  (:use #:common-lisp)
  (:shadow #:describe
           #:describe-object
           #:inspect)
  (:export #:axes
           #:cell-makunbound
           #:cell-makunbound-p
           #:cell-remove
           #:cell-remove-p
           #:cell-value
           #:cell-value-setf-p
           #:describe
           #:describe-object
           #:inspect
           #:make-cell-iterator))
