(in-package #:tazivor)

(defmethod axes ((object cons))
  (declare (ignore object))
  (list :contents))

(defmethod make-cell-iterator ((object cons) (axis (eql :contents)))
  (declare (ignore object axis))
  (make-list-iterator `(car cdr)))

(defmethod cell-value ((object cons) (axis (eql :contents)) (cell (eql 'car)))
  (declare (ignore axis cell))
  (values (car object) t t))

(defmethod cell-value ((object cons) (axis (eql :contents)) (cell (eql 'cdr)))
  (declare (ignore axis cell))
  (values (cdr object) t t))

(defmethod cell-value-setf-p ((object cons) (axis (eql :contents)) cell)
  (declare (ignore object axis cell))
  t)

(defmethod (setf cell-value) (new-value (object cons) (axis (eql :contents)) (cell (eql 'car)))
  (declare (ignore axis cell))
  (rplaca object new-value))

(defmethod (setf cell-value) (new-value (object cons) (axis (eql :contents)) (cell (eql 'cdr)))
  (declare (ignore axis cell))
  (rplacd object new-value))
