(in-package #:tazivor)

(defmethod axes ((object symbol))
  (declare (ignore object))
  `(nil class compiler-macro-function function macro-function package :plist setf variable))

(defmethod cell-value ((object symbol) axis (cell (eql :documentation)))
  (declare (ignore cell))
  (let ((doc (documentation object axis)))
    (if doc
        (values doc t t)
        (values nil nil t))))

(defmethod cell-value-setf-p ((object symbol) axis (cell (eql :documentation)))
  (declare (ignore object axis cell))
  t)

(defmethod (setf cell-value) (new-value (object symbol) axis (cell (eql :documentation)))
  (declare (ignore cell))
  (setf (documentation object axis) new-value))

;;; Class methods for symbols

(defmethod make-cell-iterator ((object symbol) (axis (eql 'class)))
  (declare (ignore object axis))
  (make-list-iterator (list :value)))

(defmethod cell-value ((object symbol) (axis (eql 'class)) (cell (eql :value)))
  (declare (ignore axis cell))
  (let ((class (find-class object nil)))
    (if class
        (values class t t)
        (values nil nil t))))

(defmethod cell-value-setf-p ((object symbol) (axis (eql 'class)) (cell (eql :value)))
  (declare (ignore object axis cell))
  t)

(defmethod (setf cell-value) (new-value (object symbol) (axis (eql 'class)) (cell (eql :value)))
  (declare (ignore axis cell))
  (setf (find-class object) new-value))

;;; Compiler macro function methods for symbols

(defmethod make-cell-iterator ((object symbol) (axis (eql 'compiler-macro-function)))
  (declare (ignore object axis))
  (make-list-iterator (list :value :lambda-list :documentation)))

(defmethod cell-value ((object symbol) (axis (eql 'compiler-macro-function)) (cell (eql :value)))
  (declare (ignore axis cell))
  (values (compiler-macro-function object) t t))

(defmethod cell-value-setf-p ((object symbol) (axis (eql 'compiler-macro-function)) (cell (eql :value)))
  (declare (ignore object axis cell))
  t)

(defmethod (setf cell-value) (new-value (object symbol) (axis (eql 'compiler-macro-function)) (cell (eql :value)))
  (declare (ignore axis cell))
  (setf (compiler-macro-function object) new-value))

(defmethod cell-value ((object symbol) (axis (eql 'compiler-macro-function)) (cell (eql :lambda-list)))
  (declare (ignore axis cell))
  (let ((fun (compiler-macro-function object)))
    (if fun
        (values (lambda-list fun) t t)
        (values nil nil t))))

(defmethod cell-value ((object symbol) (axis (eql 'compiler-macro-function)) (cell (eql :documentation)))
  (declare (ignore axis cell))
  (let ((doc (documentation object 'compiler-macro)))
    (if doc
        (values doc t t)
        (values nil nil t))))

(defmethod (setf cell-value) (new-value (object symbol) (axis (eql 'compiler-macro-function)) (cell (eql :documentation)))
  (declare (ignore cell axis))
  (setf (documentation object 'compiler-macro) new-value))

;;; Function methods for symbols

(defmethod make-cell-iterator ((object symbol) (axis (eql 'function)))
  (declare (ignore object axis))
  (make-list-iterator (list :value :lambda-list :documentation)))

(defmethod cell-value ((object symbol) (axis (eql 'function)) (cell (eql :value)))
  (declare (ignore axis cell))
  (if (fboundp object)
      (values (symbol-function object) t t)
      (values nil nil t)))

(defmethod cell-value-setf-p ((object symbol) (axis (eql 'function)) (cell (eql :value)))
  (declare (ignore object axis cell))
  t)

(defmethod (setf cell-value) (new-value (object symbol) (axis (eql 'function)) (cell (eql :value)))
  (declare (ignore axis cell))
  (setf (symbol-function object) new-value))

(defmethod cell-makunbound-p ((object symbol) (axis (eql 'function)) (cell (eql :value)))
  (declare (ignore object axis cell))
  t)

(defmethod cell-makunbound ((object symbol) (axis (eql 'function)) (cell (eql :value)))
  (declare (ignore axis cell))
  (fmakunbound object))

(defmethod cell-value ((object symbol) (axis (eql 'function)) (cell (eql :lambda-list)))
  (declare (ignore axis cell))
  (if (fboundp object)
      (values (lambda-list object) t t)
      (values nil nil t)))

;;; Macro function methods for symbols

(defmethod make-cell-iterator ((object symbol) (axis (eql 'macro-function)))
  (declare (ignore object axis))
  (make-list-iterator (list :value)))

(defmethod cell-value ((object symbol) (axis (eql 'macro-function)) (cell (eql :value)))
  (declare (ignore axis cell))
  (values (macro-function object) t t))

(defmethod cell-value-setf-p ((object symbol) (axis (eql '-macro-function)) (cell (eql :value)))
  (declare (ignore object axis cell))
  t)

(defmethod (setf cell-value) (new-value (object symbol) (axis (eql 'macro-function)) (cell (eql :value)))
  (declare (ignore axis cell))
  (setf (macro-function object) new-value))

(defmethod cell-value ((object symbol) (axis (eql 'macro-function)) (cell (eql :lambda-list)))
  (declare (ignore axis cell))
  (let ((fun (macro-function object)))
    (if fun
        (values (lambda-list fun) t t)
        (values nil nil t))))

;;; Package methods for symbols

(defmethod make-cell-iterator ((object symbol) (axis (eql 'package)))
  (declare (ignore object axis))
  (make-list-iterator (list :value)))

(defmethod cell-value ((object symbol) (axis (eql 'package)) (cell (eql :value)))
  (declare (ignore axis cell))
  (let ((pkg (find-package object)))
    (if pkg
        (values pkg t t)
        (values nil nil t))))

(defmethod cell-makunbound-p ((object symbol) (axis (eql 'package)) (cell (eql :value)))
  (declare (ignore object axis cell))
  t)

(defmethod cell-makunbound ((object symbol) (axis (eql 'package)) (cell (eql :value)))
  (declare (ignore axis cell))
  (delete-package object))

;;; Plist methods for symbols

(defmethod make-cell-iterator ((object symbol) (axis (eql :plist)))
  (declare (ignore object axis))
  (make-list-iterator (symbol-plist object) :tail #'cddr))

(defmethod cell-value ((object symbol) (axis (eql :plist)) cell)
  (multiple-value-bind (indicator value tail)
      (get-properties (symbol-plist object) (list cell))
    (declare (ignore indicator))
    (values value (and tail t) (and tail t))))

(defmethod cell-value-setf-p ((object symbol) (axis (eql :plist)) cell)
  (declare (ignore object axis cell))
  t)

(defmethod (setf cell-value) (new-value (object symbol) (axis (eql :plist)) cell)
  (declare (ignore axis cell))
  (setf (get object cell) new-value))

(defmethod cell-remove-p ((object symbol) (axis (eql :plist)) cell)
  (declare (ignore object axis cell))
  t)

(defmethod cell-remove ((object symbol) (axis (eql :plist)) cell)
  (declare (ignore axis cell))
  (remprop object cell))

;;; Setf expander methods for symbols

(defmethod make-cell-iterator ((object symbol) (axis (eql 'setf)))
  (declare (ignore object axis))
  (make-list-iterator (list :value :lambda-list :documentation)))

(defmethod cell-value ((object symbol) (axis (eql 'setf)) (cell (eql :value)))
  (declare (ignore axis cell))
  (let ((fdesc (list 'setf object)))
    (if (fboundp fdesc)
        (values (fdefinition fdesc) t t)
        (values nil nil t))))

(defmethod cell-value-setf-p ((object symbol) (axis (eql 'setf)) (cell (eql :value)))
  (declare (ignore object axis cell))
  t)

(defmethod (setf cell-value) (new-value (object symbol) (axis (eql 'setf)) (cell (eql :value)))
  (declare (ignore axis cell))
  (setf (fdefinition (list 'setf object)) new-value))

(defmethod cell-makunbound-p ((object symbol) (axis (eql 'setf)) (cell (eql :value)))
  (declare (ignore object axis cell))
  t)

(defmethod cell-makunbound ((object symbol) (axis (eql 'setf)) (cell (eql :value)))
  (declare (ignore axis cell))
  (fmakunbound (list 'setf object)))

(defmethod cell-value ((object symbol) (axis (eql 'setf)) (cell (eql :lambda-list)))
  (declare (ignore axis cell))
  (let ((fdesc (list 'setf object)))
    (if (fboundp fdesc)
        (values (lambda-list fdesc) t t)
        (values nil nil t))))

(defmethod cell-value ((object symbol) (axis (eql 'setf)) (cell (eql :documentation)))
  (declare (ignore cell))
  (let ((doc (documentation (list 'setf object) 'function)))
    (if doc
        (values doc t t)
        (values nil nil t))))

(defmethod cell-value-setf-p ((object symbol) (axis (eql 'setf)) (cell (eql :documentation)))
  (declare (ignore object axis cell))
  t)

(defmethod (setf cell-value) (new-value (object symbol) (axis (eql 'setf)) (cell (eql :documentation)))
  (declare (ignore cell))
  (setf (documentation (list 'setf object) 'function) new-value))

;;; Variable methods for symbols

(defmethod make-cell-iterator ((object symbol) (axis (eql 'variable)))
  (declare (ignore object axis))
  (make-list-iterator (list :value :documentation)))

(defmethod cell-value ((object symbol) (axis (eql 'variable)) (cell (eql :value)))
  (declare (ignore axis cell))
  (if (boundp object)
      (values (symbol-value object) t t)
      (values nil nil t)))

(defmethod cell-value-setf-p ((object symbol) (axis (eql 'variable)) (cell (eql :value)))
  (declare (ignore object axis cell))
  t)

(defmethod (setf cell-value) (new-value (object symbol) (axis (eql 'variable)) (cell (eql :value)))
  (declare (ignore axis cell))
  (setf (symbol-value object) new-value))

(defmethod cell-makunbound-p ((object symbol) (axis (eql 'variable)) (cell (eql :value)))
  (declare (ignore object axis cell))
  t)

(defmethod cell-makunbound ((object symbol) (axis (eql 'variable)) (cell (eql :value)))
  (declare (ignore axis cell))
  (makunbound object))
