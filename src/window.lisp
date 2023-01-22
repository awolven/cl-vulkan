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

(defcallback window-close-callback :void ((window :pointer))
  (glfwSetWindowShouldClose window GLFW_TRUE)
  (values))

(defun set-window-close-callback (window &optional (callback-name 'window-close-callback))
  (glfwSetWindowCloseCallback (h window) (get-callback callback-name)))

(defun find-window (handle) ;; todo: in the ffi define this slot as int or uint
  (find handle (window-registry *app*)
	:key #'h :test #'pointer-eq))

(defmethod default-window-class-for-application ((app vulkan-application-mixin))
  'vulkan-window)

(defmethod abstract-os::handle-event ((window vulkan-window-mixin) (event abstract-os::window-resize-event))
  (call-next-method)
  (recreate-swapchain window (swapchain window) nil nil)
  (values))

(defun create-vulkan-window (app device title width height &rest args)
  (apply #'make-instance (default-window-class-for-application app)
	 :app app
	 :width width
	 :height height
	 :title title
	 :device device
	 args))

;; this is a callback which happens after the native platfrom window has been created but before events start to happen
(defmethod abstract-os::initialize-window-devices ((window vulkan-window-mixin) &rest args &key device width height &allow-other-keys)
  (let ((surface (create-window-surface device window)))
    (let* ((surface-format (find-supported-format surface))
           (present-mode (get-physical-device-surface-present-mode (paired-gpu surface) surface))
	   (swapchain (create-swapchain device window width height surface-format present-mode)))
      
      (setup-framebuffers device (render-pass swapchain) swapchain)
      
      (create-frame-resources swapchain (queue-family-index surface))
      
      (values))))

(defmethod initialize-instance :after ((window vulkan-window-mixin) &rest initargs &key app &allow-other-keys)
  (push window (window-registry app))
  (values))

(defmethod destroy-os-window ((window vulkan-window))
  (let* ((app (application window))
	 (device (default-logical-device app))
	 (vkinstance (vulkan-instance app)))
    (vkDeviceWaitIdle device)
    (destroy-swapchain (swapchain window))
    (vkDestroySurfaceKHR (h vkinstance) (h (render-surface window)) (h (allocator device)))
    #+glfw(glfwDestroyWindow (h window))))


