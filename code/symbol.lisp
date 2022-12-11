(in-package #:tazivor)

(defmethod axes ((object symbol))
  (declare (ignore object))
  `(variable class))

(defmethod make-cell-iterator ((object symbol) (axis (eql 'variable)))
  (declare (ignore object axis))
  (make-list-iterator (list :value :documentation)))

(defmethod cell-value ((object symbol) (axis (eql 'variable)) (cell (eql :value)))
  (declare (ignore axis cell))
  (if (boundp object)
      (values (symbol-value object) t t)
      (values nil nil t)))

(defmethod cell-value-setf-p ((object symbol) (axis (eql 'variable)) (cell (eql :value)))
  (declare (ignore object axis cell))
  t)

(defmethod (setf cell-value) (new-value (object symbol) (axis (eql 'variable)) (cell (eql :value)))
  (declare (ignore axis cell))
  (setf (symbol-value object) new-value))

(defmethod cell-makunbound-p ((object symbol) (axis (eql 'variable)) (cell (eql :value)))
  (declare (ignore object axis cell))
  t)

(defmethod cell-makunbound ((object symbol) (axis (eql 'variable)) (cell (eql :value)))
  (declare (ignore axis cell))
  (makunbound object))

(defmethod cell-value ((object symbol) axis (cell (eql :documentation)))
  (declare (ignore cell))
  (let ((doc (documentation object axis)))
    (if doc
        (values doc t t)
        (values nil nil t))))

(defmethod cell-value-setf-p ((object symbol) axis (cell (eql :documentation)))
  (declare (ignore object axis cell))
  t)

(defmethod (setf cell-value) (new-value (object symbol) axis (cell (eql :documentation)))
  (declare (ignore cell))
  (setf (documentation object axis) new-value))

(defmethod make-cell-iterator ((object symbol) (axis (eql 'class)))
  (declare (ignore object axis))
  (make-list-iterator (list :value)))

(defmethod cell-value ((object symbol) (axis (eql 'class)) (cell (eql :value)))
  (declare (ignore axis cell))
  (let ((class (find-class object nil)))
    (if class
        (values class t t)
        (values nil nil t))))

