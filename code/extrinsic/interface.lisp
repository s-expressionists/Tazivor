(in-package #:tazivor-extrinsic)

(defvar *inspector-hook* #'tazivor:terminal-inspect)

(defvar *default-stream*)

(defgeneric describe-object (object stream)
  (:method (object stream)
    (tazivor:describe-object-impl object stream)))

(defun describe (object &optional (stream nil streamp))
  (let ((*pretty-print* t)
        (*print-right-margin* (or *print-right-margin* 80))
        (*print-circle* t))
    (describe-object object
                     (cond ((and (not streamp)
                                 (boundp '*default-stream*))
                            *default-stream*)
                           ((null stream)
                            *standard-output*)
                           ((eq t stream)
                            *terminal-io*)
                           (t
                            stream))))
  (values))

(defun inspect (object)
  (when (boundp '*inspector-hook*)
    (funcall *inspector-hook* object)))

