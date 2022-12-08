(in-package #:tazivor)

(defmethod describe-object (object stream)
  (loop for axis in (axes object)
        do (loop with first = t
                 with iterator = (make-cell-iterator object axis)
                 for (cell cellp) = (multiple-value-list (funcall iterator))
                 while cellp
                 when first
                   do (pprint axis stream)
                      (setf first nil)
                 do (pprint cell)
                    (multiple-value-bind (value boundp)
                        (cell-value object axis cell)
                      (if boundp
                          (pprint value stream)
                          (write-line "UNBOUND" stream))))))
