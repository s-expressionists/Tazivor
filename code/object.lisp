(in-package #:tazivor)

(defmethod cell-value (object axis (cell (eql 'type-of)))
  (declare (ignore axis))
  (values (type-of object) t t))
