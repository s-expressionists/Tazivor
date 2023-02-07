(in-package #:common-lisp-user)

#+sbcl (require :sb-introspect)

(defpackage #:tazivor
  (:use #:common-lisp)
  (:export #:axes
           #:cell-makunbound
           #:cell-makunbound-p
           #:cell-remove
           #:cell-remove-p
           #:cell-value
           #:cell-value-setf-p
           #:describe-object-impl
           #:make-cell-iterator
           #:terminal-inspect))
