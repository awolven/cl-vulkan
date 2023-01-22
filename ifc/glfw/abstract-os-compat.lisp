(in-package :vk)

(defun get-required-instance-extensions ()
  (with-foreign-object (p-extension-count :unsigned-int)
    (let ((pp-extensions (glfwGetRequiredInstanceExtensions p-extension-count)))
      (loop for i from 0 below (mem-aref p-extension-count :unsigned-int)
	 collect (foreign-string-to-lisp (mem-aref pp-extensions :pointer i))))))

(defun os-window-should-close-p (window)
  (not (zerop (glfwWindowShouldClose (h window)))))

(defun (setf os-window-should-close-p) (value window)
  (glfwSetWindowShouldClose (h window) (if value 1 0)))

(defun (setf os-window-title) (title window)
  (glfwSetWindowTitle (h window) title))

(defun get-os-window-pos (window)
  (with-foreign-objects ((p-x :int)
			 (p-y :int))
    (glfwGetWindowPos (h window) p-x p-y)
    (values (mem-aref p-x :int)
	    (mem-aref p-y :int))))

(defun set-os-window-pos (window x y)
  (glfwSetWindowPos (h window) (round x) (round y)))

(defun get-os-window-cursor-pos (window)
  (with-foreign-objects ((p-x :double)
			 (p-y :double))
    (glfwGetCursorPos (h window) p-x p-y)
    (values (mem-aref p-x :double)
	    (mem-aref p-y :double))))

(defun get-os-window-size (window)
  (with-foreign-objects ((p-width :int)
			 (p-height :int))
    (glfwGetWindowSize (h window) p-width p-height)
    (values (mem-aref p-width :int)
	    (mem-aref p-height :int))))

(defun focus-os-window (window)
  (glfwFocusWindow (h window)))

(defun hide-os-window (window)
  (glfwHideWindow (h window)))

(defun show-os-window (window)
  (glfwShowWindow (h window)))

(defun maximize-os-window (window)
  (glfwMaximizeWindow (h window)))

(defun restore-os-window (window)
  (glfwRestoreWindow (h window)))

(defun iconify-os-window (window)
  (glfwIconifyWindow (h window)))

(defun get-os-window-frame-size (window)
  (with-foreign-objects ((p-left :int)
			 (p-top :int)
			 (p-right :int)
			 (p-bottom :int))
    (glfwGetWindowFrameSize (h window) p-left p-top p-right p-bottom)
    (values (mem-aref p-left :int) (mem-aref p-top :int)
	    (mem-aref p-right :int) (mem-aref p-bottom :int))))

(defun get-os-window-framebuffer-size (window)
  (with-foreign-objects ((p-width :int)
			 (p-height :int))
    (glfwGetFramebufferSize (h window) p-width p-height)
    (values (mem-aref p-width :int) (mem-aref p-height :int))))

(defun set-os-window-size (window height width)
  (glfwSetWindowSize (h window) height width))

(defun set-os-window-aspect-ratio (window numer denom)
  (glfwSetWindowAspectRatio (h window) numer denom))

(defun set-os-window-size-limits (window min-width min-height max-width max-height)
  (glfwSetWindowSizeLimits (h window) min-width min-height max-width max-height))

(defun create-glfw-window-surface (instance window &key (allocator +null-allocator+))
  (with-foreign-object (p-surface 'VkSurfaceKHR)
    (check-vk-result (glfwCreateWindowSurface (h instance) (h window) (h allocator) p-surface))
    (let ((surface (make-instance 'surface
				  :handle (mem-aref p-surface 'VkSurfaceKHR)
				  :window window
				  :instance instance
				  :allocator allocator)))
      (setf (render-surface window) surface))))
