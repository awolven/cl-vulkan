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

(defun create-descriptor-pool (device &key (allocator +null-allocator+))
  (let ((dp (create-descriptor-pool-1 device allocator 1000
				  VK_DESCRIPTOR_TYPE_SAMPLER
				  VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
				  VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE
				  VK_DESCRIPTOR_TYPE_STORAGE_IMAGE
				  VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
				  VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
				  VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
				  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER
				  VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
				  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC
				  VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT)))
    (push dp (descriptor-pools device))
    dp))
			    
(defun create-descriptor-pool-1 (device allocator descriptor-count &rest descriptor-types)
  (let ((pool-size-count (length descriptor-types)))
    (with-foreign-object (p-pool-sizes '(:struct VkDescriptorPoolSize) pool-size-count)
      (loop for i from 0 for descriptor-type in descriptor-types
	          do (let ((p-pool-size (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) i)))
	               (setf (foreign-slot-value p-pool-size '(:struct VkDescriptorPoolSize) '%vk::type)
		                   descriptor-type
		                   (foreign-slot-value p-pool-size '(:struct VkDescriptorPoolSize) '%vk::descriptorCount)
		                   descriptor-count)))
      (with-vk-struct (p-pool-info VkDescriptorPoolCreateInfo)
      	(with-foreign-slots ((%vk::flags %vk::maxSets %vk::poolSizeCount %vk::pPoolSizes)
			                       p-pool-info (:struct VkDescriptorPoolCreateInfo))
	        (setf %vk::flags VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
		            %vk::maxSets (* (length descriptor-types) descriptor-count)
		            %vk::poolSizeCount pool-size-count
		            %vk::pPoolSizes p-pool-sizes)
	        (with-foreign-object (p-descriptor-pool 'VkDescriptorPool)
	          (check-vk-result (vkCreateDescriptorPool (h device) p-pool-info (h allocator) p-descriptor-pool))
	          (make-instance 'descriptor-pool :handle (mem-aref p-descriptor-pool 'VkDescriptorPool)
			                                      :device device :allocator allocator)))))))
