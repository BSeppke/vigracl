;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; vigracl.asd --- Common Lisp wrapper for the vigra Computer Vision Lib.
;;;
;;; Copyright (C) 2007-2014, Benjamin Seppke
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

(defsystem :vigracl
  :version "1.0.0"
  :description "A Common Lisp wrapper for VIGRA"
  :author "Benjamin Seppke"
  :maintainer "Benjamin Seppke"
  :licence "MIT"
  :depends-on (waaf-cffi uiop)
  :components
   ((:file "package")
    (:file "config")
	(:file "helpers")
	(:file "impex")
	(:file "imgproc")
	(:file "morphology")
	(:file "segmentation")
	(:file "filters")
	(:file "tensors")
	(:file "splineimageview")))