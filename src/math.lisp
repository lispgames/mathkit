(in-package :kit.math)

(declaim (inline v))
(defun v (l)
  (etypecase l
    (sb-cga:vec
     l)
    (cons
     (sb-cga:vec (float (pop l) 1f0) (float (pop l) 1f0) (float (pop l) 1f0)))
    (vector
     (sb-cga:vec (float (aref l 0) 1f0) (float (aref l 1) 1f0)
                 (float (aref l 2) 1f0)))))

(declaim (inline deg-to-rad rad-to-deg))
(defun deg-to-rad (x)
  (typecase x
    (single-float
     (float (* x (/ pi 180.0)) 1.9))
    (t (* x (/ pi 180)))))
(defun rad-to-deg (x)
  (typecase x
    (single-float
     (float (* x (/ 180.0 pi)) 1.0))
    (t (* x (/ 180 pi)))))

(defun frustum (left right bottom top near far)
  (let ((r-l (- right left))
        (t-b (- top bottom))
        (f-n (- far near))
        (2near (* 2 near)))
    (matrix (/ 2near r-l) 0.0 (/ (+ right left) r-l) 0.0
            0.0 (/ 2near t-b) (/ (+ top bottom) t-b) 0.0
            0.0 0.0 (- (/ (+ far near) f-n)) (/ (* -2 far near) f-n)
            0.0 0.0 -1.0 0.0)))

(defun perspective-matrix (fovy aspect z-near z-far)
  (let ((f (float (/ (tan (/ fovy 2))) 1.0))
        (dz (- z-near z-far)))
    (matrix (/ f aspect) 0.0 0.0 0.0
            0.0 f 0.0 0.0
            0.0 0.0 (/ (+ z-near z-far) dz) (/ (* 2 z-near z-far) dz)
            0.0 0.0 -1.0 0.0)))

(defun ortho-matrix (left right bottom top near far)
  (let ((r-l (float (- right left)))
        (t-b (float (- top bottom)))
        (f-n (float (- far near))))
    (matrix (/ 2 r-l) 0.0 0.0 (- (/ (+ right left) r-l))
            0.0 (/ 2 t-b) 0.0 (- (/ (+ top bottom) t-b))
            0.0 0.0 (/ -2 f-n) (- (/ (+ far near) f-n))
            0.0 0.0 0.0 1.0)))

(defun look-at (eye target up)
  (let* ((eye (v eye))
         (target (v target))
         (up (v up))
         (f (sb-cga:normalize (sb-cga:vec- target eye)))
         (s (sb-cga:normalize (sb-cga:cross-product f up)))
         (u (sb-cga:cross-product s f)))
    (matrix* (sb-cga:matrix (aref s 0) (aref s 1) (aref s 2) 0.0
                            (aref u 0) (aref u 1) (aref u 2) 0.0
                            (- (aref f 0)) (- (aref f 1)) (- (aref f 2)) 0.0
                            0.0 0.0 0.0 1.0)
             (translate* (- (aref eye 0)) (- (aref eye 1)) (- (aref eye 2))))))

(declaim (inline copy-matrix))
(defun copy-matrix (m)
  (matrix (aref m 0) (aref m 4) (aref m 8) (aref m 12)
          (aref m 1) (aref m 5) (aref m 9) (aref m 13)
          (aref m 2) (aref m 6) (aref m 10) (aref m 14)
          (aref m 3) (aref m 7) (aref m 11) (aref m 15)))



