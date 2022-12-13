(in-package #:tazivor)

(defmethod make-cell-iterator (object (axis (eql nil)))
  (declare (ignore object axis))
  (make-list-iterator '(type-of)))

(defmethod cell-value (object axis (cell (eql 'type-of)))
  (declare (ignore axis))
  (values (type-of object) t t))
