(in-package #:tazivor)

(defgeneric axes (object)
  (:method (object)
    (declare (ignore object))
    nil))

(defgeneric make-cell-iterator (object axis)
  (:method (object axis)
    (declare (ignore object axis))
    (lambda () (values nil nil))))

(defgeneric cell-name (object axis cell)
  (:method (object axis cell)
    cell))

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

(defgeneric cell-makunbound-p (object axis cell)
  (:method (object axis cell)
    (declare (ignore object axis cell))
    nil))

(defgeneric describe-axis (object axis stream))

(defgeneric describe-cell (object axis cell stream))

(defgeneric inspect-object (object stream))

(defgeneric inspect-axis (object axis stream))

(defgeneric inspect-cell (object axis cell stream))
