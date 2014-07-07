# mathkit

This is a purely math-related utility kit, providing functions which
can be useful for games, 3D, and GL in general.  Right now this
includes additional matrix functionality by @3b on top of
[sb-cga](https://github.com/nikodemus/sb-cga).

* `ortho-matrix LEFT RIGHT BOTTOM TOP NEAR FAR`: Construct an
  orthographic matrix like `glOrtho` from OpenGL 2.1.
* `frustum LEFT RIGHT BOTTOM TOP NEAR FAR`: Construct a perspective
  matrix from the given parameters, like `glFrustum` from OpenGL 2.1.
* `perspective-matrix FOVY-DEGREES ASPECT Z-NEAR Z-FAR`:  Construct a
  perspective matrix from the given parameters, like `gluPerspective`
  from OpenGL 2.1.
* `look-at EYE TARGET UP`: Where `EYE`, `TARGET`, and `UP` are
  vectors, construct a viewing matrix much like `gluLookAt` from
  OpenGL 2.1.

**Note:** These merely *create* native Lisp matrices; they do **not**
*multiply* them like the similar GL functions or in any way set them
as GL state like the old fixed-function pipeline.

Additionally:

* `copy-matrix M`: Create a copy of `M`.
* `deg-to-rad D`: Convert `D` degrees to radians.
