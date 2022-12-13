(in-package #:tazivor)

(defvar *inspector-hook*)

(defvar *default-stream*)

(defgeneric describe-object (object stream))

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

(defgeneric axes (object)
  (:method (object)
    (declare (ignore object))
    nil))

(defgeneric axis-keys (object axis)
  (:method (object axis)
    (declare (ignore object axis))
    nil))             
    
(defgeneric make-cell-iterator (object axis)
  (:method (object axis)
    (declare (ignore object axis))
    (lambda () (values nil nil))))
    
(defgeneric cell-value (object axis cell)
  (:method (object axis cell)
    (declare (ignore object axis cell))
    (values nil nil nil)))             

(defgeneric cell-value-setf-p (object axis cell)
  (:method (object axis cell)
    (declare (ignore object cell))
    nil))

(defgeneric (setf cell-value) (new-value object axis cell))

(defgeneric cell-remove-p (object axis cell)
  (:method (object axis cell)
    (declare (ignore object axis cell))
    nil))

(defgeneric cell-remove (object axis cell))

(defgeneric cell-remove-p (object axis cell)
  (:method (object axis cell)
    (declare (ignore object axis cell))
    nil))

(defgeneric cell-makunbound (object axis cell))

(defgeneric describe-axis (object axis stream))

(defgeneric describe-cell (object axis cell stream))

(defgeneric inspect-object (object stream))

(defgeneric inspect-axis (object axis stream))

(defgeneric inspect-cell (object axis cell stream))
