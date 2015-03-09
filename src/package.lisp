(defpackage #:kit.math
  (:use #:cl #:sb-cga)
  (:export #:deg-to-rad
           #:rad-to-deg
           #:matrix*vec4
           #:copy-matrix
           #:perspective-matrix
           #:ortho-matrix
           #:look-at
           #:frustum
           #:unproject

           #:vec2 #:vec3 #:vec4
           #:dvec2 #:dvec3 #:dvec4))

