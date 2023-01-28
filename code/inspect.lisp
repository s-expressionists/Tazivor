(in-package #:tazivor)

(defvar *inspect-context*)

(defclass reference ()
  ((object :reader reference-object
           :initarg :object)))

(defclass axis-reference (reference)
  ((axis :reader reference-axis
         :initarg :axis)))

(defclass cell-reference (axis-reference)
  ((cell :reader reference-cell
         :initarg :cell)))

(defclass terminal-context ()
  ((references :reader terminal-context-references
               :initform (make-array 100 :adjustable t :fill-pointer 0))
   (objects :accessor terminal-context-objects
            :initarg :objects
            :initform nil)))

(defun whitespace-char-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return)))

(defun read-command (stream)
  (write-string "> " stream)
  (finish-output stream)
  (loop for char = (read-char stream)
        when (eql char #\Newline)
          do (write-string "> " stream)
             (finish-output stream)
        unless (whitespace-char-p char)
          return (values (char-upcase char)
                         (unless (whitespace-char-p (peek-char nil stream))
                           (aref (terminal-context-references *inspect-context*)
                                 (parse-integer (with-output-to-string (s)
                                                  (loop for char = (peek-char nil stream)
                                                        until (whitespace-char-p char)
                                                        do (write-char (read-char stream) s)))))))))

(defgeneric inspect-command (stream key object)
  (:method (stream key object)
    (declare (ignore object))
    (format stream "Unknown inspector command of ~a.~%" key)
    t))

(defgeneric inspect-cell-command (stream key object axis cell)
  (:method (stream key object axis cell)
    (declare (ignore object))
    (format stream "Unknown cell inspector command of ~a.~%" key)
    t))

(defun inspect-peek (stream)
  (setf (fill-pointer (terminal-context-references *inspect-context*)) 0)
  (inspect-object (first (terminal-context-objects *inspect-context*)) stream))

(defun inspect-push (stream object)
  (setf (fill-pointer (terminal-context-references *inspect-context*)) 0)
  (push object (terminal-context-objects *inspect-context*))
  (inspect-object object stream))

(defun terminal-inspect (object)
  (loop with *pretty-print* = t
        with *print-right-margin* = (or *print-right-margin* 80)
        with *print-circle* = t
        with *inspect-context* = (make-instance 'terminal-context :objects (list object))
        for (key reference) = (multiple-value-list (read-command *query-io*))
        initially (inspect-peek *query-io*)
        finally (format *query-io* "Leaving inspection mode.~%")
        while (etypecase reference
                (null
                 (inspect-command *query-io* key
                                  (first (terminal-context-objects *inspect-context*))))
                (cell-reference
                 (inspect-cell-command *query-io* key
                                       (reference-object reference)
                                       (reference-axis reference)
                                       (reference-cell reference)))))
  (values))

(defmethod inspect-object (object stream)
  (pprint-logical-block (stream (axes object))
    (format stream "~s~:@_" object)
    (loop (pprint-exit-if-list-exhausted)
          (inspect-axis object (pprint-pop) stream))))

(defmethod inspect-axis (object axis stream)
  (terpri stream)
  (pprint-logical-block (stream nil)
    (cond ((and axis (listp axis))
           (format stream "~a~2I~:@_" (first axis))
           (loop for sub-axis in (cdr axis)
                 do (inspect-axis object sub-axis stream)))
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
                   do (inspect-cell object axis cell stream))))))

(defmethod inspect-cell (object axis cell stream)
  (with-accessors ((references terminal-context-references))
      *inspect-context*
    (pprint-logical-block (stream nil)
      (format stream "~a. ~2:I~a ↦ ~:_" (length references) cell)
      (vector-push-extend (make-instance 'cell-reference :object object
                                                         :axis axis
                                                         :cell cell)
                          references)
      (multiple-value-bind (value boundp)
          (cell-value object axis cell)
        (if boundp
            (write value :stream stream)
            (write-string "UNBOUND" stream))))
    (pprint-newline :mandatory stream)))

(defmethod inspect-cell (object axis (cell (eql :documentation)) stream)
  (with-accessors ((references terminal-context-references))
      *inspect-context*
    (pprint-logical-block (stream nil)
      (format stream "~a. ~2:I~a ↦ " (length references) cell)
      (vector-push-extend (make-instance 'cell-reference :object object
                                                         :axis axis
                                                         :cell cell)
                          references)
      (multiple-value-bind (value boundp)
          (cell-value object axis cell)
        (if boundp
            (format stream "~@:_~a" value)
            (format stream "~:_UNBOUND"))))
    (pprint-newline :mandatory stream)))

(setf *inspector-hook* #'terminal-inspect)

(defmethod inspect-command (stream (key (eql #\E)) object)
  (declare (ignore key object))
  (pprint (eval (read stream)) stream)
  t)

(defmethod inspect-command (stream (key (eql #\H)) object)
  (declare (ignore stream key object))
  (format stream "General Inspector Help~%~
                  h<integer>?        - General help or cell specific help~%~
                  i<integer>?        - Inspect current object or cell~%~
                  p                  - Inspect previous object~%~
                  s<integer> <form>  - SETF cell value~%~
                  m<integer>         - MAKUNBOUND cell value~%~
                  r<integer>         - REMOVE cell~%~
                  e <form>           - eval form~%~
                  q                  - Quit inspector~%")
  t)

(defmethod inspect-cell-command (stream (key (eql #\H)) object axis cell)
  (format stream "Inspector Help for ~a~@[ on axis ~a~]~%~
                  i<integer>         - inspect cell value~%"
          cell axis)
  (when (cell-value-setf-p object axis cell)
    (format stream "s<integer> <form>  - Set cell value~%"))
  (when (cell-makunbound-p object axis cell)
    (format stream "m<integer>         - Set cell value to unbound~%"))
  (when (cell-remove-p object axis cell)
    (format stream "r<integer>         - Remove cell~%"))
  t)

(defmethod inspect-command (stream (key (eql #\I)) object)
  (declare (ignore stream key object))
  (inspect-peek stream)
  t)

(defmethod inspect-cell-command (stream (key (eql #\I)) object axis cell)
  (declare (ignore stream key object axis cell))
  (multiple-value-bind (value boundp)
      (cell-value object axis cell)
    (cond (boundp
           (inspect-push stream value))
          (t
           (format stream "Cell ~a~@[ on axis ~a~] is UNBOUND.~%"
                   cell axis)))
    t))

(defmethod inspect-cell-command (stream (key (eql #\M)) object axis cell)
  (cond ((cell-makunbound-p object axis cell)
         (cell-makunbound object axis cell)
         (inspect-peek stream))
        (t
         (format stream "Cell ~a~@[ on axis ~a~] does not support MAKUNBOUND.~%"
                 cell axis)))
  t)

(defmethod inspect-command (stream (key (eql #\P)) object)
  (declare (ignore stream key object))
  t)

(defmethod inspect-command (stream (key (eql #\Q)) object)
  (declare (ignore stream key object))
  nil)

(defmethod inspect-cell-command (stream (key (eql #\R)) object axis cell)
  (cond ((cell-remove-p object axis cell)
         (cell-remove object axis cell)
         (inspect-peek stream))
        (format stream "Cell ~a~@[ on axis ~a~] does not support REMOVE.~%"
                cell axis))
  t)

(defmethod inspect-cell-command (stream (key (eql #\S)) object axis cell)
  (let ((form (read stream)))
    (cond ((cell-value-setf-p object axis cell)
           (setf (cell-value object axis cell) (eval form))
           (inspect-peek stream))
          (t
           (format stream "Cell ~a~@[ on axis ~a~] does not support SETF.~%"
                   cell axis)))
    t))
