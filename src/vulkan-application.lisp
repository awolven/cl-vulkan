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

(defvar *app*)

(defmethod shutdown-application (app)
  (let* ((window (first (last (window-registry app))))
	 (swapchain (swapchain window))
	 (device (device swapchain))
	 (queue-family-index (queue-family-index (render-surface window))))

    (device-wait-idle device)

    (destroy-swapchain swapchain)

    (let ((command-pool (find-command-pool device queue-family-index)))
      (when command-pool
	(loop for command-buffer across (command-buffers command-pool)
	   do (free-command-buffer command-buffer)
	   finally (setf (fill-pointer (command-buffers command-pool)) 0))

	(destroy-command-pool command-pool)))

    (destroy-frame-resources swapchain)

    (vkDestroySurfaceKHR (h (instance (render-surface window))) (h (render-surface window)) (h (allocator (instance (render-surface window)))))
    (glfwDestroyWindow (h window))
    (destroy-vulkan-instance (instance device))
    (glfwPollEvents) ;; bug in glfw3.3 on macosx mojave.
    (glfwTerminate)
    (values)))

(defmethod initialize-instance :before ((instance vulkan-application-mixin)
					&rest initargs &key &allow-other-keys)
  (setq *app* instance)
  (apply #'setup-vulkan instance initargs)
  (values))

(defmethod initialize-instance :after ((instance vulkan-application-mixin)
				       &rest initargs &key (title "VkTk Demo") (width 2560) (height 1440) &allow-other-keys)
  (declare (ignore initargs))
  (setf (main-window instance)
	(create-vulkan-window instance (default-logical-device instance) title width height))
  (values))

(defun setup-vulkan (app &rest args &key (compute-queue-count #+windows 1 #+(or darwin linux) 0)
				      (wide-lines #+windows t #+(or darwin linux) nil)
				      (rectangular-lines #+windows t #+(or darwin linux) nil)
				      (stippled-lines #+windows t #+(or darwin linux) nil)
			   &allow-other-keys)
  (let ((vulkan-instance
	 (or *vulkan-instance*
	     (apply #'create-instance #+darwin :layer-names #+darwin nil args))))
    (setf (vulkan-instance app) vulkan-instance)
    (let ((debug-callback (when (debug-report-present? vulkan-instance)
			    (create-debug-report-callback vulkan-instance 'debug-report-callback))))
      (setf (debug-callback vulkan-instance) debug-callback)
      
      (let ((physical-devices (enumerate-physical-devices vulkan-instance)))

	(setf (system-gpus app) physical-devices)
	
	(multiple-value-bind (gpu index) (block get-gpu
					   (loop for gpu in physical-devices
					      do (loop for queue-family in (queue-families gpu) for i from 0
						    do (let ((queue-flags (slot-value queue-family 'queue-flags)))
							 (when (not (zerop (logand queue-flags VK_QUEUE_GRAPHICS_BIT)))
							   (return-from get-gpu (values gpu i)))))))
	  (declare (ignore index))
	  (when (null gpu)
	    (error "No graphics device available."))
	  #+NIL(pick-graphics-gpu physical-devices surface)
      
	  (let* ((device (apply #'create-logical-device gpu
				:compute-queue-count compute-queue-count
				:device-extensions
				(list VK_KHR_SWAPCHAIN_EXTENSION_NAME #-darwin "VK_EXT_line_rasterization")
				:rectangular-lines rectangular-lines
				:stippled-lines stippled-lines
				:enable-wide-lines wide-lines
				(when (has-geometry-shader-p gpu) (list :enable-geometry-shader t)))))

	    (setf (default-logical-device app) device)

	    (unless (or (zerop compute-queue-count)
			(null compute-queue-count))
	      ;; todo: this needs to work for compute-queue-count > 1
	      (multiple-value-bind (compute-queue compute-qfi)
		  (compute-queue device)
		(declare (ignore compute-queue))
		(let ((command-pool (or (find-command-pool device compute-qfi)
					(create-command-pool device compute-qfi))))
		  (loop for i from 0 below compute-queue-count
		     do (create-command-buffer device command-pool)))))
	    (values)))))))
