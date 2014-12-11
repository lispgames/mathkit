(defpackage #:kit.math
  (:use #:cl #:sb-cga)
  (:export #:deg-to-rad
           #:copy-matrix
           #:perspective-matrix
           #:ortho-matrix
           #:look-at
           #:frustum

           #:vec2 #:vec3 #:vec4
           #:dvec2 #:dvec3 #:dvec4))

