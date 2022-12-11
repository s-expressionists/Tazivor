(in-package #:tazivor)

(defmethod axes ((object hash-table))
  (list nil :entries))

(defmethod make-cell-iterator ((object hash-table) (axis (eql :entries)))
  (with-hash-table-iterator (iterator object)
    (lambda ()
      (multiple-value-bind (entryp key value)
          (iterator)
        (declare (ignore value))
        (values key entryp)))))

(defmethod cell-value ((object hash-table) (axis (eql :entries)) cell)
  (multiple-value-bind (value valuep)
      (gethash cell object)
    (values value valuep valuep)))

(defmethod cell-value-setf-p ((object hash-table) (axis (eql :entries)) cell)
  (declare (ignore object cell))
  t)

(defmethod (setf cell-value) (new-value (object hash-table) (axis (eql :entries)) cell)
  (setf (gethash cell object) new-value))

(defmethod cell-remove-p ((object hash-table) (axis (eql :entries)) cell)
  (declare (ignore object cell))
  t)

(defmethod cell-remove ((object hash-table) (axis (eql :entries)) cell)
  (remhash cell object))

(defmethod make-cell-iterator ((object hash-table) (axis (eql nil)))
  (make-list-iterator (list 'type-of :count :rehash-size :rehash-threshold :size :test)))

(defmethod cell-value ((object hash-table) (axis (eql nil)) (cell (eql :count)))
  (values (hash-table-count object) t))

(defmethod cell-value ((object hash-table) (axis (eql nil)) (cell (eql :rehash-size)))
  (values (hash-table-rehash-size object) t))

(defmethod cell-value ((object hash-table) (axis (eql nil)) (cell (eql :rehash-threshold)))
  (values (hash-table-rehash-threshold object) t))

(defmethod cell-value ((object hash-table) (axis (eql nil)) (cell (eql :size)))
  (values (hash-table-size object) t))

(defmethod cell-value ((object hash-table) (axis (eql nil)) (cell (eql :test)))
  (values (hash-table-test object) t))
