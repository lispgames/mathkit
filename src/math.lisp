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

(defmacro floatify ((&rest symbols) &body body)
  `(let (,@(loop for symbol in symbols
                 collect `(,symbol (float ,symbol))))
     ,@body))

(declaim (inline deg-to-rad rad-to-deg))
(defun deg-to-rad (x)
  "Converts X, a number, from degrees to radians."
  (typecase x
    (single-float
     (float (* x (/ pi 180.0)) 1.0))
    (t (* x (/ pi 180)))))
(defun rad-to-deg (x)
  "Converts X, a number, from radians to degrees."
  (typecase x
    (single-float
     (float (* x (/ 180.0 pi)) 1.0))
    (t (* x (/ 180 pi)))))

(defun matrix*vec4 (matrix vector)
  (declare (type matrix matrix)
           (type vec4 vector))
  (macrolet ((a (i j) `(aref matrix ,(+ (* j 4) i)))
             (v (x) `(aref vector ,x)))
    (vec4 (+ (* (a 0 0) (v 0)) (* (a 0 1) (v 1)) (* (a 0 2) (v 2)) (* (a 0 3) (v 3)))
          (+ (* (a 1 0) (v 0)) (* (a 1 1) (v 1)) (* (a 1 2) (v 2)) (* (a 1 3) (v 3)))
          (+ (* (a 2 0) (v 0)) (* (a 2 1) (v 1)) (* (a 2 2) (v 2)) (* (a 2 3) (v 3)))
          (+ (* (a 3 0) (v 0)) (* (a 3 1) (v 1)) (* (a 3 2) (v 2)) (* (a 3 3) (v 3))))))

(setf (symbol-function 'matrix*vec3) #'transform-point)

(defun frustum (left right bottom top near far)
  "Returns a projection matrix that is similar to the glFrustum matrix.

  LEFT, RIGHT, BOTTOM, TOP, NEAR and FAR are numbers representing
  their respective clipping planes. NEAR and FAR must be positive."
  (floatify (left right bottom top near far)
    (let ((r-l (- right left))
          (t-b (- top bottom))
          (f-n (- far near))
          (2near (* 2.0 near)))
      (matrix (/ 2near r-l) 0.0 (/ (+ right left) r-l) 0.0
              0.0 (/ 2near t-b) (/ (+ top bottom) t-b) 0.0
              0.0 0.0 (- (/ (+ far near) f-n)) (/ (* -2 far near) f-n)
              0.0 0.0 -1.0 0.0))))

(defun perspective-matrix (fovy aspect z-near z-far)
  "Returns a projection matrix that is similar to the gluPerspective matrix.

  FOVY is the field of view, in degrees.

  ASPECT is the aspect ratio of the window, width / height.

  Z-NEAR and Z-FAR are positive numbers representing the depth
  clipping planes."
  (floatify (fovy aspect z-near z-far)
    (let ((f (float (/ (tan (/ fovy 2))) 1.0))
          (dz (- z-near z-far)))
      (matrix (/ f aspect) 0.0 0.0 0.0
              0.0 f 0.0 0.0
              0.0 0.0 (/ (+ z-near z-far) dz) (/ (* 2 z-near z-far) dz)
              0.0 0.0 -1.0 0.0))))

(defun ortho-matrix (left right bottom top near far)
  "Returns a projection matrix that is similar to the glOrtho matrix.

  LEFT, RIGHT, BOTTOM, TOP, NEAR and FAR are numbers representing
  their respective clipping planes."
  (floatify (left right bottom top near far)
    (let ((r-l (- right left))
          (t-b (- top bottom))
          (f-n (- far near)))
      (matrix (/ 2.0 r-l) 0.0 0.0 (- (/ (+ right left) r-l))
              0.0 (/ 2.0 t-b) 0.0 (- (/ (+ top bottom) t-b))
              0.0 0.0 (/ -2.0 f-n) (- (/ (+ far near) f-n))
              0.0 0.0 0.0 1.0))))

(defun look-at (eye target up)
  "Returns a view matrix that is similar to the gluLookAt matrix.

  EYE and TARGET are both three dimensional coordinate vectors, with
  the former representing the eye's location and the latter the center
  of its viewing target.

  UP is a direction vector, representing which way is up for the eye."
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


(defun unproject (point model-matrix perspective-matrix viewport)
  (declare (type vec3 point)
           (type matrix model-matrix perspective-matrix)
           (type vec4 viewport))
  (let* ((inv-pm (inverse-matrix
                  (matrix* perspective-matrix
                           model-matrix)))
         (new-point (vec4 (float
                           (1- (/ (* 2 (- (aref point 0) (aref viewport 0)))
                                  (aref viewport 2))))
                          (float
                           (1- (/ (* 2 (- (aref point 1) (aref viewport 1)))
                                  (aref viewport 3))))
                          (float (1- (* 2 (aref point 2))))
                          1.0))
         (obj (matrix*vec4 inv-pm new-point)))
    (vec/ (vec3 (aref obj 0) (aref obj 1) (aref obj 2))
          (aref obj 3))))
