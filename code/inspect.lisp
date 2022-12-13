(in-package #:tazivor)

(defvar *inspect-context*)

(defclass terminal-context ()
  ((cells :reader terminal-context-cells
          :initform (make-array 100 :adjustable t :fill-pointer 0))
   (objects :accessor terminal-context-objects
            :initarg :objects
            :initform nil)))

(defun read-command (stream)
  (write-string "> " stream)
  (let ((line (read-line stream nil nil)))
    (when line
      (setf line (string-trim '(#\space #\tab #\newline #\return) line))
      (let ((pos (position #\Space line)))
        (if pos
            (values (subseq line 0 pos)
                    (with-input-from-string (s (subseq line pos))
                      (loop for form = (read s nil s)
                            until (eq form s)
                            collect form)))
            (values line nil))))))

(defun terminal-help (stream &optional (item nil itemp))
  (if itemp
      (let ((args (aref (terminal-context-cells *inspect-context*) item)))
        (format stream "Inspector Help for ~a~@[ on axis ~a~]~%~
                        (inspect|i) ~a             - inspect cell value~%"
                (third args) (second args) item)
        (when (apply #'cell-value-setf-p args)
          (format stream "(setf|s) ~a <form>         - Set cell value~%" item))
        (when (apply #'cell-makunbound-p args)
          (format stream "(makunbound|m|u) ~a <form> - Set cell value to unbound~%" item))
        (when (apply #'cell-remove-p args)
          (format stream "(remove|r) ~a <form>       - Remove cell~%" item)))
      (format stream "General Inspector Help~%~
                      (help|h|?) <integer>?      - General help or cell specific help~%~
                      (inspect|i) <integer>?     - Reinspect current object or cell~%~
                      (setf|s) <integer> <form>  - SETF cell value~%~
                      (makunbound|m|u) <integer> - MAKUNBOUND cell value~%~
                      (remove|r) <integer>       - REMOVE cell~%~
                      (quit|q)                   - Quit inspector~%"))
  nil)

(defun terminal-inspect-current (stream &optional (item nil itemp))
  (when itemp    
    (push (aref (terminal-context-cells *inspect-context*) item)
          (terminal-context-objects *inspect-context*)))
  :inspect)

(defun terminal-previous (stream)
  (cond ((cdr (terminal-context-objects *inspect-context*))
         (pop (terminal-context-objects *inspect-context*))
         :inspect)
        (t
         (format stream "No previous object to inspect.~%")
         nil)))

(defun terminal-setf (stream item form)
  (let ((args (aref (terminal-context-cells *inspect-context*) item)))
    (cond ((apply #'cell-value-setf-p args)
           (setf (apply #'cell-value args) (eval form))
           :inspect)
          (t
           (format stream "Cell ~a~@[ on axis ~a~] does not support SETF.~%"
                   (third args) (second args))
           nil))))

(defun terminal-makunbound (stream item)
  (let ((args (aref (terminal-context-cells *inspect-context*) item)))
    (cond ((apply #'cell-makunbound-p args)
           (apply #'cell-makunbound args)
           :inspect)
          (t
           (format stream "Cell ~a~@[ on axis ~a~] does not support MAKUNBOUND.~%"
                   (third args) (second args))
           nil))))

(defun terminal-remove (stream item)
  (let ((args (aref (terminal-context-cells *inspect-context*) item)))
    (cond ((apply #'cell-remove-p args)
           (apply #'cell-remove args)
           :inspect)
          (t
           (format stream "Cell ~a~@[ on axis ~a~] does not support REMOVE.~%"
                   (third args) (second args))
           nil))))

(defun terminal-inspect (object)
  (let ((*pretty-print* t)
        (*print-right-margin* (or *print-right-margin* 80))
        (*print-circle* t)
        (*inspect-context* (make-instance 'terminal-context :objects (list object)))
        (commands (make-hash-table :test #'equalp)))
    (setf (gethash "?" commands) #'terminal-help
          (gethash "h" commands) #'terminal-help
          (gethash "help" commands) #'terminal-help
          (gethash "inspect" commands) #'terminal-inspect-current
          (gethash "i" commands) #'terminal-inspect-current
          (gethash "previous" commands) #'terminal-previous
          (gethash "p" commands) #'terminal-previous
          (gethash "s" commands) #'terminal-setf
          (gethash "setf" commands) #'terminal-setf
          (gethash "m" commands) #'terminal-makunbound
          (gethash "u" commands) #'terminal-makunbound
          (gethash "makunbound" commands) #'terminal-makunbound
          (gethash "r" commands) #'terminal-remove
          (gethash "remove" commands) #'terminal-remove)
    (inspect-object object *terminal-io*)
    (terpri *terminal-io*)
    (loop for (command forms) = (multiple-value-list (read-command *terminal-io*))
          for handler = (gethash command commands)
          while handler
          do (case (apply handler *terminal-io* forms)
               (:quit
                (return-from terminal-inspect))
               (:inspect
                (setf (fill-pointer (terminal-context-cells *inspect-context*)) 0)
                (inspect-object (first (terminal-context-objects *inspect-context*) *terminal-io*)
                (terpri *terminal-io*))))))
  (values))

(defmethod inspect-object (object stream)
  (pprint-logical-block (stream nil)
    (format stream "~s~:@_" object)
    (pprint-logical-block (stream (axes object))
      (loop (pprint-exit-if-list-exhausted)
            (inspect-axis object (pprint-pop) stream)))))

(defmethod inspect-axis (object axis stream)
  (pprint-logical-block (stream nil)
    (pprint-newline :mandatory stream)
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
                   do (inspect-cell object axis cell stream))))
    (pprint-indent :block 0)))

(defmethod inspect-cell (object axis cell stream)
  (with-accessors ((cells terminal-context-cells))
      *inspect-context*
    (format stream "~a. ~a ↦ ~2I~:_" (length cells) cell)
    (vector-push-extend (list object axis cell) cells)
    (multiple-value-bind (value boundp)
        (cell-value object axis cell)
      (if boundp
          (write value :stream stream)
          (write-string "UNBOUND" stream)))
    (pprint-indent :block 0 stream)
    (pprint-newline :mandatory stream)))

(defmethod inspect-cell (object axis (cell (eql :documentation)) stream)
  (with-accessors ((cells terminal-context-cells))
      *inspect-context*
    (format stream "~a. ~a ↦ " (length cells) cell)
    (vector-push-extend (list object axis cell) cells)
    (multiple-value-bind (value boundp)
        (cell-value object axis cell)
      (if boundp
          (format stream "~2I~@:_~a" value)
          (format stream "~2I~:_UNBOUND")))
    (pprint-indent :block 0 stream)
    (pprint-newline :mandatory stream)))

(setf *inspector-hook* #'terminal-inspect)
