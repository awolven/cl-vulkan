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

(defcallback error-callback :void ((error :int) (description (:pointer :char)))
  (error-callback-function error description))

(defun error-callback-function (error description)
  (format *error-output* "GLFW Error: ~A: ~A~%" error (foreign-string-to-lisp description))
  (values))

#+NIL
(defcallback window-close-callback :void ((window :pointer))
  (glfwSetWindowShouldClose window GLFW_TRUE)
  (values))

#+NIL
(defun set-window-close-callback (window &optional (callback-name 'window-close-callback))
  (glfwSetWindowCloseCallback (h window) (get-callback callback-name)))

#+glfw
(defun find-window (handle) ;; todo: in the ffi define this slot as int or uint
  (gethash handle (window-registry *app*)
	:key #'h :test #'pointer-eq))

(defmethod clim:handle-event ((window vulkan-window-mixin) (event clui::window-resize-event-mixin))
  (let ((width (clui::window-resize-event-new-width event))
	(height (clui::window-resize-event-new-height event)))
    (unless (or (zerop width) (zerop height))
      (unless (render-surface window)
	(clui::initialize-window-devices window
					 :width width
					 :height height))
      (call-next-method)
      (setf (recreate-swapchain? window) t)
      (setf (window-initialized? window) t)
      (values))))


(defmethod clui::destroy-window ((window vulkan-window))
  (destroy-os-window window))

(defmethod destroy-os-window ((window vulkan-window))
  (let* ((dpy (clui:window-display window))
	 (device (default-logical-device dpy))
	 (vkinstance *vulkan-instance*))
    (vkDeviceWaitIdle device)
    (destroy-swapchain (swapchain window))
    (vkDestroySurfaceKHR (h vkinstance) (h (render-surface window)) (h (allocator device)))))


