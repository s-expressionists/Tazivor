(in-package #:tazivor)

(defmethod axes ((object structure-object))
  (list :slots))

(defmethod make-cell-iterator ((object structure-object) (axis (eql :slots)))
  (make-list-iterator (mapcar #-abcl #'closer-mop:slot-definition-name
                              #+abcl (lambda (slot)
                                       (system::dsd-name slot))
                              (closer-mop:class-slots (class-of object)))))

(defmethod cell-value ((object structure-object) (axis (eql :slots)) cell)
  (values (slot-value object cell) t t))

(defmethod cell-value-setf-p ((object structure-object) (axis (eql :slots)) cell)
  t)

(defmethod (setf cell-value) (new-value (object structure-object) (axis (eql :slots)) cell)
  (setf (slot-value object cell) new-value))
