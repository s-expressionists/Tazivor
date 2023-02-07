(in-package #:tazivor)

(defun describe-object-impl (object stream)
  (pprint-logical-block (stream nil)
    (format stream "~s~:@_" object)
    (pprint-logical-block (stream (axes object))
      (loop (pprint-exit-if-list-exhausted)
            (describe-axis object (pprint-pop) stream)))))

(defmethod describe-axis (object axis stream)
  (pprint-logical-block (stream nil)
    (pprint-newline :mandatory stream)
    (cond ((and axis (listp axis))
           (format stream "~a~2I~:@_" (first axis))
           (loop for sub-axis in (cdr axis)
                 do (describe-axis object sub-axis stream)))
          (t
           (when axis
             (format stream "~a~:@_" axis))
           (loop with iterator = (make-cell-iterator object axis)
                 for (cell cellp) = (multiple-value-list (funcall iterator))
                 for count from 1
                 while cellp
                 if (and *print-level* (> count *print-level*))
                   do (format stream "...~:@_")
                      (loop-finish)
                 else
                   do (describe-cell object axis cell stream))))))

(defmethod describe-cell (object axis cell stream)
  (format stream "~a ↦ ~2I~:_" (cell-name object axis cell))
  (multiple-value-bind (value boundp)
      (cell-value object axis cell)
    (if boundp
        (write value :stream stream)
        (write-string "UNBOUND" stream)))
  (pprint-indent :block 0 stream)
  (pprint-newline :mandatory stream))

(defmethod describe-cell (object axis (cell (eql :documentation)) stream)
  (format stream "~a ↦ " (cell-name object axis cell))
  (multiple-value-bind (value boundp)
      (cell-value object axis cell)
    (if boundp
        (format stream "~2I~@:_~a" value)
        (format stream "~2I~:_UNBOUND")))
  (pprint-indent :block 0 stream)
  (pprint-newline :mandatory stream))
