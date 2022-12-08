(in-package #:tazivor)

(defun make-list-iterator (list)
  (lambda ()
    (if list
        (values (pop list) t)
        (values nil nil))))
