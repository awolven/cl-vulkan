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

(defun create-shader-module-from-file (device filename &key (allocator +null-allocator+))
  (multiple-value-bind (binary size) (read-shader-file filename)
    (unwind-protect (create-shader-module device binary size :allocator allocator)
      (foreign-free binary))))

(defun read-shader-file (filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
	  (byte))
      (loop while (setq byte (read-byte stream nil))
	 do (vector-push-extend byte buffer))
      (let* ((size (fill-pointer buffer))
	     (binary (foreign-alloc (list :array :unsigned-char size))))
	(loop for b across buffer for i from 0
	   do (setf (mem-aref binary :unsigned-char i) b))
	(values binary size)))))

(defun create-shader-module (device p-code size &key (allocator +null-allocator+))
  (with-vk-struct (p-create-info VkShaderModuleCreateInfo)
    (with-foreign-slots ((%vk::codeSize
			  %vk::pCode)
			 p-create-info
			 (:struct VkShaderModuleCreateInfo))
      (setf %vk::codeSize size
	    %vk::pCode p-code)
      (with-foreign-object (p-shader-module 'VkShaderModule)
	(check-vk-result (vkCreateShaderModule (h device) p-create-info (h allocator) p-shader-module))
	(make-instance 'shader-module :handle (mem-aref p-shader-module 'VkShaderModule)
		       :device device :allocator allocator)))))

(defun destroy-shader-module (shader-module)
  (let ((device (device shader-module))
	(allocator (allocator shader-module)))
    (vkDestroyShaderModule (h device) (h shader-module) (h allocator))
    (values)))
