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

(defun get-queue-family-index-with-wsi-support (gpu surface)
  ;; Check for WSI support
  (loop for i from 0 below (length (queue-families gpu))
     do (with-foreign-object (p-res 'VkBool32)
	  (check-vk-result (vkGetPhysicalDeviceSurfaceSupportKHR (h gpu) i (h surface) p-res))
	  (when (eq (mem-aref p-res 'VkBool32) VK_TRUE)
	      (return i)))
     finally (error "No WSI support on physical device")))

(defun supports-presentation-mode-p (surface presentation-mode)
  (loop for mode in (presentation-modes surface)
     when (eq mode presentation-mode)
     do (return t)))

(cffi:defcfun ("glfwCreateWindowSurface" glfwCreateWindowSurface) VkResult
  (instance VkInstance)
  (window :pointer)
  (allocator (:pointer (:struct VkAllocationCallbacks)))
  (surface (:pointer VkSurfaceKHR)))

(defun create-window-surface (instance window &key (allocator +null-allocator+))
  (with-foreign-object (p-surface 'VkSurfaceKHR)
    (check-vk-result (glfwCreateWindowSurface (h instance) (h window) (h allocator) p-surface))
    (let ((surface (make-instance 'surface
				  :handle (mem-aref p-surface 'VkSurfaceKHR)
				  :window window
				  :instance instance
				  :allocator allocator)))
      (setf (render-surface window) surface))))      

(defun initialize-window-surface (surface gpu queue-family-index)
  (setf (paired-gpu surface) gpu)
  (setf (supported-formats surface) (get-surface-formats gpu surface))
  (setf (presentation-modes surface) (get-present-modes gpu surface))
  (setf (queue-family-index surface) queue-family-index)
  t)

(defun pick-graphics-gpu (gpus surface)
  (loop for gpu in gpus
     do (let ((index (get-queue-family-index-with-wsi-support gpu surface)))
	  (when index (return (values gpu index))))
     finally (error "Could not find a gpu with window system integration support.")))
