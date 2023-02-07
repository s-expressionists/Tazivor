(in-package #:common-lisp-user)

(defpackage #:tazivor-extrinsic
  (:use #:common-lisp)
  (:shadow #:describe
           #:describe-object
           #:inspect)
  (:export #:*inspector-hook*
           #:describe
           #:describe-object
           #:inspect))
