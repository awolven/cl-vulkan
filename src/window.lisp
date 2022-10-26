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

(defmethod window-class (app)
  'window)

(defun create-window (app &key title width height)

  (assert (typep width 'integer))
  (assert (typep height 'integer))

  (when (zerop (glfwInit))
    (error "GLFW failed to initialize."))

  (glfwSetErrorCallback (get-callback 'error-callback))
  
  (when (zerop (glfwVulkanSupported))
    (error "GLFW: Vulkan Not Supported."))  
    
  (glfwWindowHint GLFW_CLIENT_API GLFW_NO_API)
  
  (let ((window
	 (make-instance (window-class app)
			:app app
			:handle (glfwCreateWindow width height title +nullptr+ +nullptr+))))

    (push window (window-registry app))

    (set-framebuffer-size-callback window)
    (set-window-close-callback window)
    
    ;;(glfwSetWindowUserPointer (h window) (h window))
    
    window))

(defun create-vulkan-window (app device title width height)
  (let* ((window (create-window app :width width :height height :title title))
	       (surface (create-window-surface (vulkan-instance app) window))
	       (gpu (physical-device device))
	       (index (get-queue-family-index-with-wsi-support gpu surface)))
    (initialize-window-surface surface gpu index)
    (let* ((surface-format (find-supported-format surface))
           (present-mode (get-physical-device-surface-present-mode gpu surface))
	         (swapchain (create-swapchain device window width height surface-format present-mode)))
      
      (setup-framebuffers device (render-pass swapchain) swapchain)
      
      (setf (default-descriptor-pool app) (create-descriptor-pool device))
      
      (create-frame-resources swapchain index)
      
      window)))

(defun destroy-window (window)
  (let* ((app (application window))
	 (device (default-logical-device app))
	 (vkinstance (vulkan-instance app)))
    (vkDeviceWaitIdle device)
    (destroy-swapchain (swapchain window))
    (vkDestroySurfaceKHR (h vkinstance) (h (render-surface window)) (h (allocator device)))
    (glfwDestroyWindow (h window))))

;; todo, make sure to put a glfwTerminate in destroy-application
;; maybe put glfwInit in create-application

(defun window-should-close-p (window)
  (not (zerop (glfwWindowShouldClose (h window)))))

(defun (setf window-should-close-p) (value window)
  (glfwSetWindowShouldClose (h window) (if value 1 0)))

(defun (setf window-title) (title window)
  (glfwSetWindowTitle (h window) title))

(defun get-window-pos (window)
  (with-foreign-objects ((p-x :int)
			 (p-y :int))
    (glfwGetWindowPos (h window) p-x p-y)
    (values (mem-aref p-x :int)
	    (mem-aref p-y :int))))

(defun set-window-pos (window x y)
  (glfwSetWindowPos (h window) (round x) (round y)))

(defun get-cursor-pos (window)
  (with-foreign-objects ((p-x :double)
			 (p-y :double))
    (glfwGetCursorPos (h window) p-x p-y)
    (values (mem-aref p-x :double)
	    (mem-aref p-y :double))))

(defun get-window-size (window)
  (with-foreign-objects ((p-width :int)
			 (p-height :int))
    (glfwGetWindowSize (h window) p-width p-height)
    (values (mem-aref p-width :int)
	    (mem-aref p-height :int))))

(defun focus-window (window)
  (glfwFocusWindow (h window)))

(defun hide-window (window)
  (glfwHideWindow (h window)))

(defun show-window (window)
  (glfwShowWindow (h window)))

(defun maximize-window (window)
  (glfwMaximizeWindow (h window)))

(defun restore-window (window)
  (glfwRestoreWindow (h window)))

(defun iconify-window (window)
  (glfwIconifyWindow (h window)))

(defun window-frame-size (window)
  (with-foreign-objects ((p-left :int)
			 (p-top :int)
			 (p-right :int)
			 (p-bottom :int))
    (glfwGetWindowFrameSize (h window) p-left p-top p-right p-bottom)
    (values (mem-aref p-left :int) (mem-aref p-top :int)
	    (mem-aref p-right :int) (mem-aref p-bottom :int))))

(defun get-framebuffer-size (window)
  (with-foreign-objects ((p-width :int)
			 (p-height :int))
    (glfwGetFramebufferSize (h window) p-width p-height)
    (values (mem-aref p-width :int) (mem-aref p-height :int))))

(defun set-window-size (window height width)
  (glfwSetWindowSize (h window) height width))

(defun set-window-aspect-ratio (window numer denom)
  (glfwSetWindowAspectRatio (h window) numer denom))

(defun set-window-size-limits (window min-width min-height max-width max-height)
  (glfwSetWindowSizeLimits (h window) min-width min-height max-width max-height))
