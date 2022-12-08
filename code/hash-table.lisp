(in-package #:tazivor)

(defmethod axes ((object hash-table))
  (list :contents :metadata))

(defmethod make-cell-iterator ((object hash-table) (axis (eql :contents)))
  (with-hash-table-iterator (iterator object)
    (lambda ()
      (multiple-value-bind (entryp key value)
          (iterator)
        (declare (ignore value))
        (values key entryp)))))

(defmethod cell-value ((object hash-table) (axis (eql :contents)) cell)
  (multiple-value-bind (value valuep)
      (gethash cell object)
    (values value valuep valuep)))

(defmethod cell-value-setf-p ((object hash-table) (axis (eql :contents)) cell)
  (declare (ignore object cell))
  t)

(defmethod (setf cell-value) (new-value (object hash-table) (axis (eql :contents)) cell)
  (setf (gethash cell object) new-value))

(defmethod cell-remove-p ((object hash-table) (axis (eql :contents)) cell)
  (declare (ignore object cell))
  t)

(defmethod cell-remove ((object hash-table) (axis (eql :contents)) cell)
  (remhash cell object))

(defmethod make-cell-iterator ((object hash-table) (axis (eql :metadata)))
  (make-list-iterator (list :count :rehash-size :rehash-threshold :size :test)))

(defmethod cell-value ((object hash-table) (axis (eql :metadata)) (cell (eql :count)))
  (values (hash-table-count object) t))

(defmethod cell-value ((object hash-table) (axis (eql :metadata)) (cell (eql :rehash-size)))
  (values (hash-table-rehash-size object) t))

(defmethod cell-value ((object hash-table) (axis (eql :metadata)) (cell (eql :rehash-threshold)))
  (values (hash-table-rehash-threshold object) t))

(defmethod cell-value ((object hash-table) (axis (eql :metadata)) (cell (eql :size)))
  (values (hash-table-size object) t))

(defmethod cell-value ((object hash-table) (axis (eql :metadata)) (cell (eql :test)))
  (values (hash-table-test object) t))
