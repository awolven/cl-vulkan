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

(defun create-frame-resources (swapchain queue-family-index &key (allocator +null-allocator+))
  (let* ((device (device swapchain))
	 (queued-frames (number-of-images swapchain))
	 (array (make-array queued-frames)))	 
    (flet ((create-semaphore ()
	     (with-vk-struct (p-info VkSemaphoreCreateInfo)
	       (with-foreign-objects ((p-semaphore 'VkSemaphore))
		 (check-vk-result
		  (vkCreateSemaphore (h device) p-info (h allocator) p-semaphore))
		 (make-instance 'semaphore
				:handle (mem-aref p-semaphore 'VkSemaphore)
				:device device
				:allocator allocator)))))
      (setf (frame-resources swapchain) array)
      (loop for i from 0 below queued-frames
	 do (setf (aref array i)
		  (let ((command-pool (create-command-pool device queue-family-index)))
		    (make-instance 'frame-resources
				   :fence (with-vk-struct (p-info VkFenceCreateInfo)
					    (with-foreign-slots ((%vk::flags)
								 p-info
								 (:struct VkFenceCreateInfo))
					      (setf %vk::flags VK_FENCE_CREATE_SIGNALED_BIT)
					      (with-foreign-object (p-fence 'VkFence)
						(check-vk-result (vkCreateFence (h device) p-info (h allocator) p-fence))
						(make-instance 'fence :handle (mem-aref p-fence 'VkFence)
							       :device device :allocator allocator))))
				   :present-complete-semaphore (create-semaphore)
				   :render-complete-semaphore (create-semaphore)
				   :command-buffer (create-command-buffer device command-pool :allocator allocator)
				   :command-pool command-pool))))
      array)))

(defun destroy-frame-resources (swapchain)
  (let ((device (device swapchain)))
    (when (frame-resources swapchain)
      (loop for frame-resource across (frame-resources swapchain)
	 do (vkDestroyFence (h device) (h (fence frame-resource)) (h (allocator (fence frame-resource))))
	   (let ((sem (render-complete-semaphore frame-resource)))
	     (vkDestroySemaphore (h device) (h sem) (h (allocator sem))))
	   (let ((sem (present-complete-semaphore frame-resource)))
	     (vkDestroySemaphore (h device) (h sem) (h (allocator sem))))
	   (let ((command-pool (frame-command-pool frame-resource)))
	     (free-command-buffers command-pool)
	     (destroy-command-pool command-pool))
	 finally (setf (frame-resources swapchain) nil)))))

(defun create-swapchain (device window width height surface-format present-mode
			 &key (allocator +null-allocator+)
			   (old-swapchain nil)
			   (image-usage VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT)
			   (image-sharing-mode VK_SHARING_MODE_EXCLUSIVE)
			   (pre-transform VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR)
			   (composite-alpha VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR)
			   (clipped-p t))
  (with-slots (surface) window
    (with-vk-struct (p-create-info VkSwapchainCreateInfoKHR)
      (with-foreign-slots ((%vk::surface
			    %vk::imageFormat
			    %vk::imageColorSpace
			    %vk::imageArrayLayers
			    %vk::imageUsage
			    %vk::imageSharingMode
			    %vk::preTransform
			    %vk::compositeAlpha
			    %vk::presentMode
			    %vk::clipped
			    %vk::oldSwapchain
			    %vk::minImageCount)
			   p-create-info
			   (:struct VkSwapchainCreateInfoKHR))
	(setf %vk::surface (h surface)
	      %vk::imageFormat (surface-format-format surface-format)
	      %vk::imageColorSpace (surface-format-color-space surface-format)
	      %vk::imageArrayLayers 1 ;; todo: lookup what this means.
	      %vk::imageUsage image-usage
	      %vk::imageSharingMode image-sharing-mode
	      %vk::preTransform pre-transform
	      %vk::compositeAlpha composite-alpha
	      %vk::presentMode present-mode
	      %vk::clipped (if (or (not clipped-p) (and (integerp clipped-p) (zerop clipped-p))) VK_FALSE VK_TRUE)
	      %vk::oldSwapchain (if old-swapchain (h old-swapchain) +nullptr+))

	(let* ((capabilities (get-physical-device-surface-capabilities-khr
			      (physical-device device) surface))
	       (cap-min-image-count (min-image-count capabilities))
	       (cap-max-image-count (max-image-count capabilities)))
	
	  (if (> cap-max-image-count 0)
	      (setf %vk::minImageCount (min (+ cap-min-image-count 1) cap-max-image-count))
	      (setf %vk::minImageCount (+ cap-min-image-count 1)))
	
	  (let ((cap-current-extent-width (capabilities-current-extent-width capabilities))
		(fb-width)
		(fb-height))
	  
	    (if (eq cap-current-extent-width #xffffffff)
		(setf fb-width width
		      fb-height height)

		(setf fb-width cap-current-extent-width
		      fb-height (capabilities-current-extent-height capabilities)))

	    (setf (foreign-slot-value
		   (foreign-slot-pointer p-create-info '(:struct VkSwapchainCreateInfoKHR) '%vk::imageExtent)
		   '(:struct VkExtent2D)
		   '%vk::width) fb-width

		   (foreign-slot-value
		    (foreign-slot-pointer p-create-info '(:struct VkSwapchainCreateInfoKHR) '%vk::imageExtent)
		    '(:struct VkExtent2D)
		    '%vk::height) fb-height)

	    (with-foreign-object (p-swapchain 'VkSwapchainKHR)
	      (check-vk-result (vkCreateSwapchainKHR (h device) p-create-info (h allocator) p-swapchain))
	      (when old-swapchain (vkDestroySwapchainKHR (h device) (h old-swapchain) (h allocator)))
	      (let* ((render-pass (create-render-pass device surface-format))
		     (swapchain (make-instance 'swapchain :handle (mem-aref p-swapchain 'VkSwapchainKHR)
					       :device device
					       :width fb-width
					       :height fb-height
					       :surface-format surface-format
					       :allocator allocator
					       :render-pass render-pass)))
		(initialize-swapchain swapchain window)
		swapchain))))))))

(defun initialize-swapchain (swapchain window)
  (setf (swapchain window) swapchain)
  (setf (images swapchain) (get-swapchain-images-khr swapchain))
  (setf (color-image-views swapchain) (create-image-views swapchain))
  (setf (depth-image swapchain) (create-depth-image (device swapchain)
						    (fb-width swapchain)
						    (fb-height swapchain)))
  (setf (depth-image-view swapchain) (create-depth-image-view (device swapchain)
							      (depth-image swapchain)))
  swapchain)

(defun destroy-swapchain-resources (swapchain)
  (with-slots (device) swapchain

    (device-wait-idle device)

    (when (depth-image-view swapchain)
      (destroy-image-view (depth-image-view swapchain))
      (setf (depth-image-view swapchain) nil))

    (when (depth-image swapchain)
      (destroy-image (depth-image swapchain))
      (setf (depth-image swapchain) nil))
	
    (loop for image-view across (color-image-views swapchain)
       do (destroy-image-view image-view)
	 (setf (color-image-views swapchain) nil))
	
    (destroy-framebuffers swapchain)
	
    (when (render-pass swapchain)
      (destroy-render-pass (render-pass swapchain))
      (setf (render-pass swapchain) nil))
	
    ;; destroys semaphores and fence
    (destroy-frame-resources swapchain)
    (values)))

(defun recreate-swapchain (window swapchain fb-width fb-height)
  (loop while (or (zerop fb-width) (zerop fb-height))
     do (glfwWaitEvents))
  
  (if (not (or (zerop fb-width) (zerop fb-height)))
      (with-slots (application) window
	(with-slots (device) swapchain
	  (let* ((surface (render-surface window))
		 (queue-family-index (queue-family-index surface)))
	    ;;(free-command-buffers command-pool)
	    (when swapchain
	      (destroy-swapchain-resources swapchain))
	      
	    (let ((surface-format (find-supported-format (render-surface window)))
		  (present-mode VK_PRESENT_MODE_FIFO_KHR
		    #+NOTYET(get-physical-device-surface-present-mode
				 (physical-device device) (render-surface window)))
		  (old-swapchain swapchain))
		
	      (setf swapchain (create-swapchain device window fb-width fb-height
						surface-format present-mode
						:old-swapchain old-swapchain))

		
	      (setup-framebuffers device (render-pass swapchain) swapchain)
	      (create-frame-resources swapchain queue-family-index))))))
  (values))

(defun destroy-swapchain (swapchain)
  (with-slots (device) swapchain
    (with-slots (allocator) device
      (destroy-swapchain-resources swapchain)
      (unless (null-pointer-p (h swapchain))
	(vkDestroySwapchainKHR (h device) (h swapchain) (h allocator)))))
  (values))

(defun frame-begin (swapchain render-pass current-frame clear-value command-pool)
  (declare (ignore command-pool))
  (let* ((device (device swapchain))
	 (frame-resource (elt (frame-resources swapchain) current-frame))
	 (command-buffer (frame-command-buffer frame-resource))
	 (fence (fence frame-resource))
	 (present-complete-sem (present-complete-semaphore frame-resource))
	 (image-index))

    (with-foreign-object (p-fence 'VkFence)
      (setf (mem-aref p-fence 'VkFence) (h fence))
      
	(tagbody

	 continue
	 
	   (let ((result (vkWaitForFences (h device) 1 p-fence VK_TRUE 100)))
	     ;; probably can set wait time to uint32 max and eliminate this tagbody
	     (when (eq result VK_SUCCESS)
	       (go break))
	     (when (eq result VK_TIMEOUT)
	       (go continue))
	     (check-vk-result result))
	
	 break
	   
	   ;;(check-vk-result (vkResetFences (h device) 1 p-fence))
	   ;; reset all the command buffers from pool
	   ;;(reset-command-pool device command-pool)
	   ))

    (with-foreign-object (p-back-buffer-index :uint32)
	(check-vk-result (vkAcquireNextImageKHR (h device)
						(h swapchain)
						UINT64_MAX
						(h present-complete-sem)
						VK_NULL_HANDLE
						p-back-buffer-index))
      
	(setf image-index (mem-aref p-back-buffer-index :uint32)))

    
	   
      (begin-command-buffer command-buffer)

      (with-vk-struct (p-viewports VkViewport)
	(with-foreign-slots ((%vk::x
			      %vk::y
			      %vk::width
			      %vk::height
			      %vk::minDepth
			      %vk::maxDepth)
			     p-viewports (:struct VkViewport))
	  (setf %vk::x 0.0f0
		%vk::y 0.0f0
		%vk::width (coerce (fb-width swapchain) 'single-float)
		%vk::height (coerce (fb-height swapchain) 'single-float)
		%vk::minDepth 0.0f0
		%vk::maxDepth 1.0f0))

	(vkCmdSetViewport (h command-buffer) 0 1 p-viewports))

      (with-vk-struct (p-scissor VkRect2D)
	(setf (foreign-slot-value
	       (foreign-slot-pointer p-scissor '(:struct VkRect2D) '%vk::offset)
	       '(:struct VkOffset2D)
	       '%vk::x) 0
	     
	       (foreign-slot-value
		(foreign-slot-pointer p-scissor '(:struct VkRect2D) '%vk::offset)
		'(:struct VkOffset2D)
		'%vk::y) 0
	      
	       (foreign-slot-value
		(foreign-slot-pointer p-scissor '(:struct VkRect2D) '%vk::extent)
		'(:struct VkExtent2D)
		'%vk::width) (fb-width swapchain)
	       
	       (foreign-slot-value
		(foreign-slot-pointer p-scissor '(:struct VkRect2D) '%vk::extent)
		'(:struct VkExtent2D)
		'%vk::height) (fb-height swapchain))

	(vkCmdSetScissor (h command-buffer) 0 1 p-scissor))

      (with-vk-struct (p-info VkRenderPassBeginInfo)
      
	(with-foreign-slots ((%vk::renderPass
			      %vk::framebuffer
			      %vk::renderArea
			      %vk::clearValueCount
			      %vk::pClearValues)
			     p-info
			     (:struct VkRenderPassBeginInfo))

	  (with-foreign-object (p-clear-values '(:union VkClearValue) 2)
	    (setf (mem-aref (mem-aptr p-clear-values '(:union VkClearValue) 0) :float 0) (elt clear-value 0)
		  (mem-aref (mem-aptr p-clear-values '(:union VkClearValue) 0) :float 1) (elt clear-value 1)
		  (mem-aref (mem-aptr p-clear-values '(:union VkClearValue) 0) :float 2) (elt clear-value 2)
		  (mem-aref (mem-aptr p-clear-values '(:union VkClearValue) 0) :float 3) (elt clear-value 3)

		  (foreign-slot-value
		   (foreign-slot-pointer (mem-aptr p-clear-values '(:union VkClearValue) 1)
					 '(:union VkClearValue)
					 '%vk::depthStencil)
		   '(:struct VkClearDepthStencilValue)
		   '%vk::depth) 1.0f0
		 
		  (foreign-slot-value
		   (foreign-slot-pointer (mem-aptr p-clear-values '(:union VkClearValue) 1)
					 '(:union VkClearValue)
					 '%vk::depthStencil)
		   '(:struct VkClearDepthStencilValue)
		   '%vk::stencil) 0)

	    (setf %vk::renderPass (h render-pass)
		  %vk::framebuffer (h (elt (framebuffers swapchain) image-index))

		  (foreign-slot-value
		   (foreign-slot-pointer
		    (foreign-slot-pointer p-info '(:struct VkRenderPassBeginInfo) '%vk::renderArea)
		    '(:struct VkRect2D) '%vk::extent)
		   '(:struct VkExtent2D)
		   '%vk::width) (fb-width swapchain)

		  (foreign-slot-value
		   (foreign-slot-pointer
		    (foreign-slot-pointer p-info '(:struct VkRenderPassBeginInfo) '%vk::renderArea)
		    '(:struct VkRect2D) '%vk::extent)
		   '(:struct VkExtent2D)
		   '%vk::height) (fb-height swapchain)
		 		  
		  %vk::clearValueCount 2
		  %vk::pClearValues p-clear-values)
	  
	    (vkCmdBeginRenderPass (h command-buffer) p-info VK_SUBPASS_CONTENTS_INLINE))))
      image-index))

(defun frame-end (swapchain queue current-frame)
  (let* ((device (device swapchain))
	 (frame-resource (elt (frame-resources swapchain) current-frame))
	 (fence (fence frame-resource))
	 (current-command-buffer (frame-command-buffer frame-resource))
	 (present-complete-sem (present-complete-semaphore frame-resource))
	 (render-complete-sem (render-complete-semaphore frame-resource)))

    (vkCmdEndRenderPass (h current-command-buffer))

    ;; End recording of command buffer
    (check-vk-result (vkEndCommandBuffer (h current-command-buffer)))

    ;; set the state of the fence to unsignaled
    (with-foreign-object (p-fence 'VkFence)
      (setf (mem-aref p-fence 'VkFence) (h fence))
      (check-vk-result (vkResetFences (h device) 1 p-fence)))

    ;; execute the command buffer
    (queue-submit queue current-command-buffer present-complete-sem render-complete-sem fence)
    
    (values)))

(defun frame-present (swapchain queue current-frame image-index window)
  (let ((frame-resource (elt (frame-resources swapchain) current-frame)))
    
    (with-foreign-objects ((p-indices :uint32)
			   (p-swapchain 'VkSwapchainKHR)
			   (p-wait-semaphores 'VkSemaphore))
	      
      (setf (mem-aref p-indices :uint32) image-index
	    (mem-aref p-swapchain 'VkSwapchainKHR) (h swapchain)
	    (mem-aref p-wait-semaphores 'VkSemaphore)
	    (h (render-complete-semaphore frame-resource)))
    
      (with-vk-struct (p-info VkPresentInfoKHR)
	(with-foreign-slots ((%vk::waitSemaphoreCount
			      %vk::pWaitSemaphores
			      %vk::swapchainCount
			      %vk::pSwapchains
			      %vk::pImageIndices)
			     p-info
			     (:struct VkPresentInfoKHR))
	
	  (setf %vk::waitSemaphoreCount 1
		%vk::pWaitSemaphores p-wait-semaphores
		%vk::swapchainCount 1
		%vk::pSwapchains p-swapchain
		%vk::pImageIndices p-indices)
	
	  (let ((result (vkQueuePresentKHR (h queue) p-info)))
	  
	    (if (or (eq result VK_ERROR_OUT_OF_DATE_KHR) (eq result VK_SUBOPTIMAL_KHR))
		(multiple-value-bind (fb-width fb-height) (get-framebuffer-size window)
		  (recreate-swapchain window swapchain fb-width fb-height))
		(check-vk-result result)))))))

  (values))
