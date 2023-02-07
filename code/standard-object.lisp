(in-package #:tazivor)

(defmethod axes ((object standard-object))
  (list nil :slots))

(defmethod make-cell-iterator ((object standard-object) (axis (eql nil)))
  (make-list-iterator '(:type)))

(defmethod cell-value ((object standard-object) (axis (eql nil)) (cell (eql :type)))
  (values (type-of object) t t))

(defmethod make-cell-iterator ((object standard-object) (axis (eql :slots)))
  (make-list-iterator (closer-mop:class-slots (class-of object))))

(defmethod cell-name ((object standard-object) (axis (eql :slots)) (cell closer-mop:slot-definition))
  (closer-mop:slot-definition-name cell))

(defmethod cell-value ((object standard-object) (axis (eql :slots)) (cell closer-mop:slot-definition))
  (let ((class (class-of object)))
    (if (closer-mop:slot-boundp-using-class class object cell)
        (values (closer-mop:slot-value-using-class class object cell) t t)
        (values nil nil t))))

(defmethod cell-value-setf-p ((object standard-object) (axis (eql :slots)) (cell closer-mop:slot-definition))
  t)

(defmethod (setf cell-value) (new-value (object standard-object) (axis (eql :slots)) (cell closer-mop:slot-definition))
  (setf (closer-mop:slot-value-using-class (class-of object) object cell) new-value))

(defmethod cell-makunbound-p ((object standard-object) (axis (eql :slots)) (cell closer-mop:slot-definition))
  t)

(defmethod cell-makunbound ((object standard-object) (axis (eql :slots)) (cell closer-mop:slot-definition))
  (closer-mop:slot-makunbound-using-class (class-of object) object cell))
