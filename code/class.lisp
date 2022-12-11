(in-package #:tazivor)

(in-package #:tazivor)

(defmethod axes ((object class))
  (list* nil
         (when (closer-mop:class-finalized-p object)
           (list (list* :slots (closer-mop:class-slots object))))))               

(defmethod make-cell-iterator ((object class) (axis (eql nil)))
  (make-list-iterator (list :name :direct-superclasses :precedence-list :default-initargs :direct-default-initargs)))

(defmethod cell-value ((object class) (axis (eql nil)) (cell (eql :name)))
  (values (class-name object) t t))

(defmethod cell-value ((object class) (axis (eql nil)) (cell (eql :direct-superclasses)))
  (values (closer-mop:class-direct-superclasses object) t t))

(defmethod cell-value ((object class) (axis (eql nil)) (cell (eql :precedence-list)))
  (if (closer-mop:class-finalized-p object)
      (values (closer-mop:class-precedence-list object) t t)
      (values nil nil t)))

(defmethod cell-value ((object class) (axis (eql nil)) (cell (eql :default-initargs)))
  (if (closer-mop:class-finalized-p object)
      (values (closer-mop:class-default-initargs object) t t)
      (values nil nil t)))

(defmethod cell-value ((object class) (axis (eql nil)) (cell (eql :direct-default-initargs)))
  (declare (ignore axis cell))
  (values (closer-mop:class-direct-default-initargs object) t t))

(defmethod make-cell-iterator ((object class) (axis closer-mop:slot-definition))
  (declare (ignore object axis))
  (make-list-iterator (list :name :type :allocation :initargs :initform :documentation)))

(defmethod cell-value ((object class) (axis closer-mop:slot-definition) (cell (eql :name)))
  (declare (ignore object cell))
  (values (closer-mop:slot-definition-name axis) t t))

(defmethod cell-value ((object class) (axis closer-mop:slot-definition) (cell (eql :type)))
  (declare (ignore object cell))
  (values (closer-mop:slot-definition-type axis) t t))

(defmethod cell-value ((object class) (axis closer-mop:slot-definition) (cell (eql :allocation)))
  (declare (ignore object cell))
  (values (closer-mop:slot-definition-allocation axis) t t))

(defmethod cell-value ((object class) (axis closer-mop:slot-definition) (cell (eql :initargs)))
  (declare (ignore object cell))
  (values (closer-mop:slot-definition-initargs axis) t t))

(defmethod cell-value ((object class) (axis closer-mop:slot-definition) (cell (eql :initform)))
  (declare (ignore object cell))
  (values (closer-mop:slot-definition-initform axis) t t))

(defmethod cell-value ((object class) (axis closer-mop:slot-definition) (cell (eql :documentation)))
  (declare (ignore object cell))
  (values (documentation object t) t t))
