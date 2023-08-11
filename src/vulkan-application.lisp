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

(defmethod shutdown-application ((dpy vulkan-enabled-display-mixin))
  (let* ((device (default-logical-device dpy)))

    (device-wait-idle device)

    (destroy-memory-pools dpy)

    (vkDestroyDescriptorPool (h device)
                             vk::VK_NULL_HANDLE
                             +nullptr+)

    (let ((command-pools (command-pools device)))
      (mapcar #'(lambda (cons)
		  (let ((command-pool (cadr cons)))
		    (when command-pool
		      (loop for command-buffer across (command-buffers command-pool)
			    do (free-command-buffer command-buffer)
			    finally (setf (fill-pointer (command-buffers command-pool)) 0))
		      (destroy-command-pool command-pool))))
	      command-pools))

    (when (next-method-p)
      (call-next-method))

    (vkDestroyDevice (h device) (h (allocator device))) 
    (values)))

(defmethod required-vulkan-device-extensions ((display vulkan-enabled-display-mixin))
  (list #-darwin "VK_EXT_line_rasterization"))

(defmethod initialize-instance :before ((instance vulkan-enabled-display-mixin)
					&rest initargs &key &allow-other-keys)
  (let ((vulkan-device-extensions (getf initargs :vulkan-device-extensions)))
    (remf initargs :vulkan-device-extensions)
    (setq vulkan-device-extensions
	  (append (required-vulkan-device-extensions instance)
		  vulkan-device-extensions))
    (apply #'setup-vulkan instance :vulkan-device-extensions vulkan-device-extensions initargs)
    (initialize-buffer-memory-pool instance)
    (values)))

(defun setup-vulkan (dpy &rest args
		     &key (compute-queue-count 0)
		       (vulkan-device-extensions nil)
		       (wide-lines #+(or windows linux) t #+(or darwin) nil)
		       (rectangular-lines nil)
		       (stippled-lines #+(or windows linux) t #+(or darwin) nil)
		     &allow-other-keys)
  (let ((vulkan-instance (get-vulkan-instance dpy)))
    (let ((debug-callback (when (debug-report-present? vulkan-instance)
			    (create-debug-report-callback vulkan-instance 'debug-report-callback))))
      (setf (debug-callback vulkan-instance) debug-callback)
      
      (let ((physical-devices (enumerate-physical-devices dpy)))

	(setf (system-gpus dpy) physical-devices)
	
	(multiple-value-bind (gpu index) (block get-gpu
					   (loop for gpu in physical-devices
					         do (loop for queue-family in (queue-families gpu) for i from 0
						          do (let ((queue-flags (slot-value queue-family 'queue-flags)))
							       (when (not (zerop (logand queue-flags VK_QUEUE_GRAPHICS_BIT)))
							         (return-from get-gpu (values gpu i)))))))
	  ;;(declare (ignore index))
	  (when (null gpu)
	    (error "No graphics device available."))
	  #+NIL(pick-graphics-gpu physical-devices surface)
      
	  (let* ((device (apply #'create-logical-device dpy gpu
				:compute-queue-count compute-queue-count
				:device-extensions
				(list* VK_KHR_SWAPCHAIN_EXTENSION_NAME
				       vulkan-device-extensions)
				:rectangular-lines rectangular-lines
				:stippled-lines stippled-lines
				:enable-wide-lines wide-lines
				:enable-geometry-shader (has-geometry-shader-p gpu)
				args)))

	    (setf (default-logical-device dpy) device)

	    (let ((command-pool (create-command-pool device index)))
	      (push (list index command-pool) (command-pools device))
	      (create-command-buffer device command-pool))

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
	    
	    (setf (default-descriptor-pool dpy) (create-descriptor-pool device))
	    
	    (values)))))))
