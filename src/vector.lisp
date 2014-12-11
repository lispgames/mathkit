(in-package :kit.math)

(defmacro define-vecn (n type &optional (prefix ""))
  (let ((vecn (alexandria:symbolicate (string-upcase prefix)
                                      'vec (format nil "~A" n))))
    `(progn
       (deftype ,vecn () '(simple-array ,type (,n)))
       (defun ,vecn (a &rest r)
         (etypecase a
           (vector
            (cond
              ((= (length a) ,n) a)
              ((> (length a) ,n)
               (let ((a+ (make-array ,n :element-type ',type)))
                 (replace a+ a)
                 a+))
              (t (let* ((a+ (make-array ,n :element-type ',type)))
                   (replace a+ a)
                   (replace a+ r :start1 (length a))
                   a+))))
           (,type
            (let* ((a+ (make-array ,n :element-type ',type)))
              (setf (aref a+ 0) a)
              (replace a+ r :start1 1)
              a+)))))))

(define-vecn 2 single-float)
(define-vecn 3 single-float)
(define-vecn 4 single-float)

(define-vecn 2 double-float "d")
(define-vecn 3 double-float "d")
(define-vecn 4 double-float "d")
