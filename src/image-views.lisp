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

(defun create-image-view (device image &key (allocator +null-allocator+)
					 (view-type VK_IMAGE_VIEW_TYPE_2D)
					 (format VK_FORMAT_R8G8B8A8_UNORM)
					 (aspect-mask VK_IMAGE_ASPECT_COLOR_BIT)
					 (base-mip-level 0)
					 (level-count 1)
					 (base-array-layer 0)
					 (layer-count 1))
  (with-vk-struct (p-view-info VkImageViewCreateInfo)
    (with-foreign-slots ((%vk::image
			  %vk::viewType
			  %vk::format)
			 p-view-info
			 (:struct VkImageViewCreateInfo))
      (with-foreign-slots ((%vk::aspectMask
			    %vk::baseMipLevel %vk::levelCount
			    %vk::baseArrayLayer %vk::layerCount)
			   (foreign-slot-pointer p-view-info
						 '(:struct VkImageViewCreateInfo)
						 '%vk::subresourceRange)
			   (:struct VKImageSubresourceRange))
	  (setf %vk::image (h image)
		%vk::viewType view-type
		%vk::format format
		
		%vk::aspectMask aspect-mask
		%vk::baseMipLevel base-mip-level
		%vk::levelCount level-count
		%vk::baseArrayLayer base-array-layer
		%vk::layerCount layer-count)

	  (with-foreign-object (p-image-view 'VkImageView)
	    (check-vk-result (vkCreateImageView (h device) p-view-info (h allocator) p-image-view))
	    (make-instance 'image-view :handle (mem-aref p-image-view 'VkImageView)
			   :device device
			   :image image
			   :allocator allocator))))))

(defun create-depth-image-view (device image &key (allocator +null-allocator+)
					       (format (find-supported-depth-format
							(physical-device device)))
					       (aspect-mask VK_IMAGE_ASPECT_DEPTH_BIT))
  (create-image-view device image :format format :aspect-mask aspect-mask :allocator allocator))
   
(defun create-image-views (swapchain &key
				       (allocator +null-allocator+)
				       (view-type VK_IMAGE_VIEW_TYPE_2D)
				       (aspect-mask VK_IMAGE_ASPECT_COLOR_BIT)
				       (base-mip-level 0)
				       (level-count 1)
				       (base-array-layer 0)
				       (layer-count 1))
    
  (let* ((count (number-of-images swapchain))
	 (image-views (make-array count)))
    (with-vk-struct (p-image-range VkImageSubresourceRange)
      (with-foreign-slots ((%vk::aspectMask
			    %vk::baseMipLevel
			    %vk::levelCount
			    %vk::baseArrayLayer
			    %vk::layerCount)
			   p-image-range
			   (:struct VkImageSubresourceRange))
		
	(setf %vk::aspectMask aspect-mask
	      %vk::baseMipLevel base-mip-level
	      %vk::levelCount level-count
	      %vk::baseArrayLayer base-array-layer
	      %vk::layerCount layer-count))

      (with-vk-struct (p-create-info VkImageViewCreateInfo)
	(with-foreign-slots ((%vk::sType
			      %vk::viewType
			      %vk::format
			      %vk::subresourceRange)
			     p-create-info
			     (:struct VkImageViewCreateInfo))
	  (with-foreign-slots ((%vk::r %vk::g %vk::b %vk::a)
			       (foreign-slot-pointer p-create-info '(:struct VkImageViewCreateInfo)
						     '%vk::components)
			       (:struct VkComponentMapping))
		   
	    (setf %vk::viewType view-type
		  %vk::format (surface-format-format (surface-format swapchain))
		  %vk::r VK_COMPONENT_SWIZZLE_R
		  %vk::g VK_COMPONENT_SWIZZLE_G
		  %vk::b VK_COMPONENT_SWIZZLE_B
		  %vk::a VK_COMPONENT_SWIZZLE_A
		  %vk::subresourceRange p-image-range)))
	
	(loop for i from 0 below count
	   do (setf (foreign-slot-value p-create-info '(:struct VkImageViewCreateInfo) '%vk::image)
		    (h (elt (images swapchain) i)))
	     (with-foreign-object (p-image-view 'VkImageView)
	       (check-vk-result (vkCreateImageView (h (device swapchain)) p-create-info (h allocator) p-image-view))
	       (setf (elt image-views i) (make-instance 'image-view
							:device (device swapchain)
							:handle (mem-aref p-image-view 'VkImageView)
							:image (elt (images swapchain) i)
							:allocator allocator))))))
    image-views))

(defun destroy-image-view (image-view)
  (vkDestroyImageView (h (device image-view)) (h image-view) (h (allocator image-view)))
  (values))
