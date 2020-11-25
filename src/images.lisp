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

(defun get-swapchain-images-khr (swapchain)
  (with-foreign-object (p-back-buffer-count :uint32)
    (check-vk-result (vkGetSwapchainImagesKHR (h (device swapchain)) (h swapchain) p-back-buffer-count +nullptr+))
    (setf (number-of-images swapchain) (mem-aref p-back-buffer-count :uint32))
    (with-foreign-object (p-back-buffers 'VkImage (mem-aref p-back-buffer-count :uint32))
      (check-vk-result (vkGetSwapchainImagesKHR (h (device swapchain)) (h swapchain) p-back-buffer-count p-back-buffers))
      (let* ((count (mem-aref p-back-buffer-count :uint32))
	     (images (make-array count)))
	(loop for i from 0 below count
	   do (setf (elt images i) (make-instance 'image :handle (mem-aref p-back-buffers 'VkImage i))))
	images))))

(defun create-image (device width height &key (allocator +null-allocator+)
					   (image-class 'image)
					   (format VK_FORMAT_R8G8B8A8_UNORM)
					   (tiling VK_IMAGE_TILING_OPTIMAL)
					   (usage (logior VK_IMAGE_USAGE_SAMPLED_BIT
							  VK_IMAGE_USAGE_TRANSFER_DST_BIT))
					   (memory-properties VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT))
  (with-vk-struct (p-info VkImageCreateInfo)
    (with-foreign-slots ((%vk::imageType
			  %vk::mipLevels
			  %vk::arrayLayers
			  %vk::format
			  %vk::tiling
			  %vk::initialLayout
			  %vk::usage
			  %vk::samples
			  %vk::sharingMode)
			 p-info (:struct VkImageCreateInfo))
      (setf %vk::imageType VK_IMAGE_TYPE_2D
	    %vk::format format
	    %vk::mipLevels 1
	    %vk::arrayLayers 1
	    %vk::samples VK_SAMPLE_COUNT_1_BIT
	    %vk::tiling tiling
	    %vk::usage usage
	    %vk::sharingMode VK_SHARING_MODE_EXCLUSIVE
	    %vk::initialLayout VK_IMAGE_LAYOUT_UNDEFINED

	    (foreign-slot-value
	     (foreign-slot-pointer p-info '(:struct VkImageCreateInfo) '%vk::extent)
	     '(:struct VkExtent3D) '%vk::width) width

	     (foreign-slot-value
	     (foreign-slot-pointer p-info '(:struct VkImageCreateInfo) '%vk::extent)
	     '(:struct VkExtent3D) '%vk::height) height

	     (foreign-slot-value
	      (foreign-slot-pointer p-info '(:struct VkImageCreateInfo) '%vk::extent)
	      '(:struct VkExtent3D) '%vk::depth) 1)

      (with-foreign-object (p-image 'VkImage)
	(check-vk-result (vkCreateImage (h device) p-info (h allocator) p-image))
	(with-vk-struct (p-req VkMemoryRequirements)
	  (vkGetImageMemoryRequirements (h device) (mem-aref p-image 'VkImage) p-req)
	  (with-vk-struct (p-alloc-info VkMemoryAllocateInfo)
	    (with-foreign-slots ((%vk::size
				  %vk::memoryTypeBits)
				 p-req
				 (:struct VkMemoryRequirements))
	      (with-foreign-slots ((%vk::allocationSize
				    %vk::memoryTypeIndex)
				   p-alloc-info
				   (:struct VkMemoryAllocateInfo))
		(setf %vk::allocationSize %vk::size
		      %vk::memoryTypeIndex (find-memory-type (physical-device device)
							    %vk::memoryTypeBits
							    memory-properties))))
	    (with-foreign-object (p-memory 'VkDeviceMemory)
	      (check-vk-result (vkAllocateMemory (h device) p-alloc-info (h allocator) p-memory))
	      (vkBindImageMemory (h device) (mem-aref p-image 'VkImage) (mem-aref p-memory 'VkDeviceMemory) 0)
	      (make-instance image-class 
			     :handle (mem-aref p-image 'VkImage)
			     :memory (make-instance 'allocated-memory
						    :handle (mem-aref p-memory 'VkDeviceMemory)
						    :device device
						    :allocator allocator)
			     :device device :allocator allocator))))))))

(defun create-depth-image (device width height &key (allocator +null-allocator+)
						 (format (find-supported-depth-format
							  (physical-device device)))
						 (tiling VK_IMAGE_TILING_OPTIMAL)
						 (usage VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT)
						 (memory-properties VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT))
  (create-image device width height
		:image-class 'depth-image
		:allocator allocator
		:format format
		:tiling tiling
		:usage usage
		:memory-properties memory-properties))

(defun destroy-image (image)
  (vkDestroyImage (h (device image)) (h image) (h (allocator image)))
  (vkFreeMemory (h (device image)) (h (allocated-memory image)) (h (allocator (allocated-memory image))))
  (values))

(defmethod transition-image-layout (device image command-pool &key format old-layout new-layout)
  (let ((command-buffer (begin-single-time-commands device command-pool)))
    (with-vk-struct (p-barrier VkImageMemoryBarrier)
      (with-foreign-slots ((%vk::oldLayout
			    %vk::newLayout
			    %vk::srcQueueFamilyIndex
			    %vk::dstQueueFamilyIndex
			    %vk::srcAccessMask
			    %vk::dstAccessMask
			    %vk::image)
			   p-barrier (:struct VkImageMemoryBarrier))
	(setf %vk::oldLayout old-layout
	      %vk::newLayout new-layout
	      %vk::srcQueueFamilyIndex VK_QUEUE_FAMILY_IGNORED
	      %vk::dstQueueFamilyIndex VK_QUEUE_FAMILY_IGNORED
	      %vk::image image)

	(if (eq new-layout VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL)
	    (progn
	      (setf (foreign-slot-value
		     (foreign-slot-pointer
		      p-barrier '(:struct VkImageMemoryBarrier)
		      '%vk::subresourceRange)
		     '(:struct VKImageSubresourceRange)
		     '%vk::aspectMask)
		    VK_IMAGE_ASPECT_DEPTH_BIT)
	      (when (has-stencil-component-p format)
		(setf (foreign-slot-value
		       (foreign-slot-pointer
			p-barrier '(:struct VkImageMemoryBarrier)
			'%vk::subresourceRange)
		       '(:struct VKImageSubresourceRange)
		       '%vk::aspectMask)
		      (logior (foreign-slot-value
			       (foreign-slot-pointer
				p-barrier '(:struct VkImageMemoryBarrier)
				'%vk::subresourceRange)
			       '(:struct VKImageSubresourceRange)
			       '%vk::aspectMask)
			      VK_IMAGE_ASPECT_STENCIL_BIT))))
	    (setf (foreign-slot-value
		   (foreign-slot-pointer
		    p-barrier '(:struct VkImageMemoryBarrier)
		    '%vk::subresourceRange)
		   '(:struct VKImageSubresourceRange)
		   '%vk::aspectMask)
		  VK_IMAGE_ASPECT_COLOR_BIT))

	(with-foreign-slots ((%vk::baseMipLevel
			      %vk::levelCount
			      %vk::baseArrayLayer
			      %vk::layerCount)
			     (foreign-slot-pointer
			      p-barrier '(:struct VkImageMemoryBarrier)
			      '%vk::subresourceRange)
			     (:struct VkImageSubresourceRange))
	  (setf %vk::baseMipLevel 0
		%vk::levelCount 1
		%vk::baseArrayLayer 0
		%vk::layerCount 1))

	(let ((source-stage)
	      (destination-stage))
	  (if (and (eq old-layout VK_IMAGE_LAYOUT_UNDEFINED)
		   (eq new-layout VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL))
	      (setf %vk::srcAccessMask 0
		    %vk::dstAccessMask VK_ACCESS_TRANSFER_WRITE_BIT
		    
		    source-stage VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
		    destination-stage VK_PIPELINE_STAGE_TRANSFER_BIT)
	      (if (and (eq old-layout VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL)
		       (eq new-layout VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))
		  (setf %vk::srcAccessMask VK_ACCESS_TRANSFER_WRITE_BIT
			%vk::dstAccessMask VK_ACCESS_SHADER_READ_BIT
			
			source-stage VK_PIPELINE_STAGE_TRANSFER_BIT
			destination-stage VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT)
		  (if (and (eq old-layout VK_IMAGE_LAYOUT_UNDEFINED)
			   (eq new-layout VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL))

		      (setf %vk::srcAccessMask 0
			    %vk::dstAccessMask (logior VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT
						      VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT)
			    source-stage VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
			    destination-stage VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT)
		      (error "unsupported layout transition"))))
	  (vkCmdPipelineBarrier (h command-buffer) source-stage	destination-stage
				0 0 +nullptr+ 0 +nullptr+ 1 p-barrier)
	  ;; todo: change first second first device-queues device to something sane
	  (end-single-time-commands device command-pool (first (second (first (device-queues device)))) command-buffer))))))
