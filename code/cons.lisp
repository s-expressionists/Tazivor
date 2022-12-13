(in-package #:tazivor)

(defmethod axes ((object cons))
  (declare (ignore object))
  (list nil :components))

(defmethod make-cell-iterator ((object cons) (axis (eql :components)))
  (declare (ignore object axis))
  (make-list-iterator `(car cdr)))

(defmethod cell-value ((object cons) (axis (eql :components)) (cell (eql 'car)))
  (declare (ignore axis cell))
  (values (car object) t t))

(defmethod cell-value ((object cons) (axis (eql :components)) (cell (eql 'cdr)))
  (declare (ignore axis cell))
  (values (cdr object) t t))

(defmethod cell-value-setf-p ((object cons) (axis (eql :components)) cell)
  (declare (ignore object axis cell))
  t)

(defmethod (setf cell-value) (new-value (object cons) (axis (eql :components)) (cell (eql 'car)))
  (declare (ignore axis cell))
  (rplaca object new-value))

(defmethod (setf cell-value) (new-value (object cons) (axis (eql :components)) (cell (eql 'cdr)))
  (declare (ignore axis cell))
  (rplacd object new-value))
