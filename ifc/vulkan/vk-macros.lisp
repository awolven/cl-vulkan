;; Copyright 2019 Andrew Kenneth Wolven <awolven@gmail.com>
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

(cl:in-package #:%vk)

(cl:defmacro defvkinstextfun ((cname lname) result-type cl:&body args)
  (cl:let ((instance-arg (cl:gensym "INSTANCE")))
    `(cl:defun ,lname (,instance-arg ,@(cl:mapcar 'cl:car args))
       (cffi:foreign-funcall-pointer
	(cffi:with-foreign-string (p-native ,cname)
	  (VkGetInstanceProcAddr ,instance-arg p-native))
        cl:nil
        ,@(cl:loop for arg in args
		   collect (cl:second arg) collect (cl:first arg))
        ,result-type))))

(cl:defmacro defvkdevextfun ((cname lname) result-type cl:&body args)
  (cl:let ((device-arg (cl:gensym "DEVICE")))
    `(cl:defun ,lname (,device-arg ,@(cl:mapcar 'cl:car args))
       (cffi:foreign-funcall-pointer
	(cffi:with-foreign-string (p-native ,cname)
	  (VkGetDeviceProcAddr ,device-arg p-native))
        cl:nil
        ,@(cl:loop for arg in args
		   collect (cl:second arg) collect (cl:first arg))
        ,result-type))))

(cl:defun zero-struct (p struct-typespec)
  (cl:loop for i from 0 below (cffi:foreign-type-size struct-typespec)
     do (cl:setf (cffi:mem-aref p :unsigned-char i) 0))
  (cl:values))

(cl:defmacro with-vk-struct ((p-info struct-type) cl:&body body)
  `(cffi:with-foreign-object (,p-info '(:struct ,struct-type))
     (zero-struct ,p-info '(:struct ,struct-type))
     ,@(cl:when (cl:gethash struct-type *s-type-table*)
	 `((cl:setf (cffi:foreign-slot-value ,p-info '(:struct ,struct-type) 'sType)
		 ,(cl:gethash struct-type *s-type-table*))))
     ,@body))
