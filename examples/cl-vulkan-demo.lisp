(in-package :cl-vulkan-demo)

(defclass demo-application (vulkan-application-mixin)
  ((exit? :initform nil :accessor app-exit?)
   (renderer :reader renderer)))

(defmethod initialize-instance :after ((instance demo-application) &rest initargs &key)
  (declare (ignore initargs))
  (with-slots (renderer) instance
    (setf renderer (make-instance 'triangle-list-renderer
				  :app instance
				  :name :triangle-list-renderer)))
  (values))

(defmethod render ((app demo-application) command-buffer frame-index
		   model-matrix view-matrix projection-matrix
		   width height)
  (render (renderer app)
	  command-buffer frame-index model-matrix
	  view-matrix projection-matrix width height)
  (values))

(defun draw-rgb-triangle (app)
  (append-colored-triangle
   (renderer app)
   (vec3 -0.25 -0.25 0.0)
   #(1.0f0 0.0f0 0.0f0 1.0f0)
   (vec3 0.25 -0.25 0.0)
   #(0.0f0 1.0f0 0.0f0 1.0f0)
   (vec3 0.0 0.5 0.0)
   #(0.0f0 0.0f0 1.0f0 1.0f0)))

(defmethod shutdown-application ((app demo-application))
  (device-wait-idle (default-logical-device app))
  (destroy-renderer (renderer app))
  (call-next-method))

(defmethod main ((app demo-application) &rest args)
  (declare (ignore args))

  (flet ((real-main ()
	   (let* ((device (default-logical-device app))
		  (main-window (main-window app))
		  (index (queue-family-index (render-surface main-window)))
		  (queue (find-queue device index))
		  (command-pool (find-command-pool device index))
		  (command-buffer (elt (command-buffers command-pool) 0)))

	     (device-wait-idle device)

	     (reset-command-pool device command-pool)

	     (begin-command-buffer command-buffer)

	     ;; one time commands here.

	     (end-command-buffer command-buffer)

	     (queue-submit1 queue command-buffer)

	     (device-wait-idle device)

	     (let ((current-frame 0)
		   (image-index)
		   (model-matrix (meye 4))
		   (view-matrix (meye 4))
		   (projection-matrix (meye 4)))

	       (with-slots (exit?) app

		 (draw-rgb-triangle app)
		  
		 (loop while (zerop (glfwWindowShouldClose (h main-window)))
		    until exit?
		    do
		      (glfwPollEvents)

		      (when (recreate-swapchain? main-window)
			(multiple-value-bind (width height) (get-framebuffer-size main-window)
			  (recreate-swapchain main-window (swapchain main-window) width height)))

		      (let* ((swapchain (swapchain main-window))
			     (command-buffer (frame-command-buffer
					      (elt (frame-resources swapchain) current-frame))))

			(setq image-index
			      (frame-begin swapchain (render-pass swapchain)
					   current-frame (clear-value main-window)
					   command-pool))

			(render app command-buffer current-frame model-matrix
				view-matrix projection-matrix
				(vk::fb-width swapchain) (vk::fb-height swapchain))

			(frame-end swapchain queue current-frame)

			(frame-present swapchain queue current-frame image-index main-window)

			(setq current-frame (mod (1+ current-frame) (number-of-images swapchain))))))))))
    
    (real-main)

    (shutdown-application app)))

(defun run (&key (width 640) (height 480))
  (flet ((runit ()
	   #+SBCL
	   (sb-int:with-float-traps-masked (:invalid
					    :inexact
					    :overflow
					    :underflow
					    :divide-by-zero)
				  
	     (main (make-instance 'demo-application :height height :width width)))

	     #-SBCL
	     (main (make-instance 'demo-application :height height :width width))))

    #+(and darwin sbcl)
    (sb-thread:interrupt-thread (sb-thread:main-thread) #'runit)

    #-darwin
    (runit)))
  
