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


(defmethod clui::destroy-window ((window vulkan-window-mixin))
  (destroy-os-window window))

(defmethod destroy-os-window ((window vulkan-window-mixin))
  (let* ((dpy (clui:window-display window))
	 (device (default-logical-device dpy))
	 (vkinstance *vulkan-instance*))
    (vkDeviceWaitIdle device)
    (destroy-swapchain (swapchain window))
    (vkDestroySurfaceKHR (h vkinstance) (h (render-surface window)) (h (allocator device)))))

(defmethod clui:initialize-window-devices ((window vulkan-window-mixin) &rest args &key width height &allow-other-keys)
  (declare (ignore args))
  (let* ((device (default-logical-device (clui:window-display window)))
	 (surface (create-window-surface device window)))
    (let* ((surface-format (find-supported-format
			    surface
			    :requested-image-format (window-desired-format window)
			    :requested-color-space (window-desired-color-space window)))
           (present-mode (get-physical-device-surface-present-mode (paired-gpu surface) surface))
	   (render-pass (display-default-render-pass (clui:window-display window))))
      
      (setf (render-pass window) render-pass)

      (let ((swapchain (create-swapchain device window width height surface-format present-mode)))
	(setf (swapchain window) swapchain)

	(setup-framebuffers device render-pass swapchain)
      
	(create-frame-resources device window (number-of-images swapchain) (queue-family-index surface))

	(with-slots (queue command-pool) window
	  (let ((index (queue-family-index surface)))
	    (setf queue (acquire-queue device VK_QUEUE_GRAPHICS_BIT))
	    (setf command-pool (find-command-pool device index))))
      
	(values)))))

