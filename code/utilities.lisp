(in-package #:tazivor)

(defun make-list-iterator (list)
  (lambda ()
    (if list
        (values (pop list) t)
        (values nil nil))))

(defun lambda-list (sym)
  #+ccl
    (ccl:arglist sym)
  #+clisp
    (system::arglist sym)
  #+(or clasp ecl)
    (ext:function-lambda-list sym)
  #+sbcl
    (sb-introspect:function-lambda-list sym)
  #+lispworks
    (lispworks:function-lambda-list sym)
  #-(or ccl clisp clasp ecl lispworks sbcl)
    (second (function-lambda-expression (or (macro-function sym)
                                            (fdefinition sym)))))
