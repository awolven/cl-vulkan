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

(defcallback debug-report-callback VkBool32 ((flags VkDebugReportFlagsEXT) (object-type VkDebugReportObjectTypeEXT)
					     (object :uint64) (location size-t) (message-code :int32)
					     (p-layer-prefix (:pointer :char)) (p-message (:pointer :char))
					     (p-user-data :pointer))
  (debug-report-function flags object-type object location message-code p-layer-prefix p-message p-user-data))

(defun debug-report-function (flags object-type object location message-code p-layer-prefix p-message p-user-data)
  (declare (ignore flags object location message-code p-layer-prefix p-user-data))
  (format *error-output* "[vulkan] ObjectType: ~A~%Message: ~A~%~%" object-type
	  (foreign-string-to-lisp p-message))
  (finish-output *error-output*)
  VK_FALSE)

(defclass debug-report-callback (handle-mixin)
  ((callback-name :initarg :callback-name
		  :reader callback-name)
   (instance :initarg :instance :reader instance)
   (allocator :initarg :allocator :reader allocator)))

(defun create-debug-report-callback (instance callback-name
				     &key (allocator +null-allocator+)
				       (flags (logior VK_DEBUG_REPORT_ERROR_BIT_EXT
						      VK_DEBUG_REPORT_WARNING_BIT_EXT
						      VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT)))
  (assert (typep instance 'instance))
  ;; create the debug report callback
  (with-vk-struct (p-debug-report-create-info VkDebugReportCallbackCreateInfoEXT)
    (with-foreign-slots ((%vk::flags %vk::pfnCallback %vk::pUserData)
			 p-debug-report-create-info (:struct VkDebugReportCallbackCreateInfoEXT))
      (setf %vk::flags flags
	    %vk::pfnCallback (get-callback callback-name)
	    %vk::pUserData +nullptr+)
      (with-foreign-object (p-debug-report 'VkDebugReportCallbackEXT)
	(check-vk-result (vkCreateDebugReportCallbackEXT (h instance) (h instance)
							 p-debug-report-create-info
							 (h allocator) p-debug-report))
	(let ((callback (make-instance 'debug-report-callback
				       :handle (mem-aref p-debug-report 'VkDebugReportCallbackEXT)
				       :callback-name callback-name
				       :instance instance
				       :allocator allocator)))
	  callback)))))

(defmethod destroy-debug-report-callback ((callback debug-report-callback))
  (vkDestroyDebugReportCallbackEXT (h (instance callback)) (h (instance callback)) (h callback) (h (allocator callback))))
