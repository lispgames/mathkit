(defpackage :mathkit.asdf
  (:use #:cl #:asdf))

(in-package :mathkit.asdf)

(defsystem :mathkit
  :description "Various utilities for math"
  :author ("3b")
  :license "MIT"
  :version "0.0"

  :depends-on (:alexandria :sb-cga)
  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "math")
   (:file "quat")))
