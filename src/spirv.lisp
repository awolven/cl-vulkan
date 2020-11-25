;; Copyright 2019, 2020 Andrew Kenneth Wolven
;; 
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :vk)

(cffi:defcfun ("compile_to_spv" compile_to_spv) :int
  (kind :int)
  (program :string)
  (bytes :pointer)
  (length :pointer))

(defstruct spirv
  (code)
  (length))

(defun compile-to-spirv (program &key (kind :vertex-shader))
  (let ((enum-kind (ecase kind
		     (:vertex-shader 0)
		     (:fragment-shader 1))))
    (with-foreign-objects ((pp-bytes :pointer)
			   (p-length :int64))
      (when (zerop (compile_to_spv enum-kind program pp-bytes p-length))
	(make-spirv :code (cffi:mem-aref pp-bytes :pointer)
		    :length (cffi:mem-aref p-length :int64))))))
