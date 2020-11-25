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

(defun create-descriptor-set-layout (device &key (allocator +null-allocator+)
					      (bindings (list (make-instance 'uniform-buffer-for-vertex-shader-dsl-binding))))

  (let ((count (length bindings)))
    (let ((p-bindings (foreign-alloc '(:struct VkDescriptorSetLayoutBinding) :count count)))
      (unwind-protect
	   (progn
	     (loop for i from 0 below count
		do (zero-struct (mem-aptr p-bindings '(:struct VkDescriptorSetLayoutBinding) i) '(:struct VkDescriptorSetLayoutBinding)))
	     (loop for binding in bindings
		for i from 0
		do (assert (typep binding 'descriptor-set-layout-binding))
		  (with-foreign-slots ((%vk::binding
					%vk::descriptorType
					%vk::descriptorCount
					%vk::stageFlags
					%vk::pImmutableSamplers)
				       (mem-aptr p-bindings '(:struct VkDescriptorSetLayoutBinding) i)
				       (:struct VkDescriptorSetLayoutBinding))
		    (let* ((is-count (length (immutable-samplers binding)))
			   (p-immutable-samplers (foreign-alloc 'VkSampler :count is-count)))
			(loop for is in (immutable-samplers binding) for i from 0
			   do (setf (mem-aref p-immutable-samplers 'VkSampler i) (h is)))
			(setf %vk::binding (binding binding)
			      %vk::descriptorType (descriptor-type binding)
			      %vk::descriptorCount (descriptor-count binding)
			      %vk::stageFlags (stage-flags binding)
			      %vk::pImmutableSamplers (if (immutable-samplers binding)
							 p-immutable-samplers
							 VK_NULL_HANDLE)))))
	     (let ((p-layout-info (foreign-alloc '(:struct VkDescriptorSetLayoutCreateInfo))))
	       (zero-struct p-layout-info '(:struct VkDescriptorSetLayoutCreateInfo))
	       (with-foreign-slots ((%vk::sType
				     %vk::bindingCount
				     %vk::pBindings)
				    p-layout-info (:struct VkDescriptorSetLayoutCreateInfo))
		 (setf %vk::sType VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
		       %vk::bindingCount count
		       %vk::pBindings p-bindings))
	       (with-foreign-object (p-descriptor-set-layout 'VkDescriptorSetLayout 1)
		 (check-vk-result (vkCreateDescriptorSetLayout (h device) p-layout-info (h allocator) p-descriptor-set-layout))
		 (make-instance 'descriptor-set-layout :handle (mem-aref p-descriptor-set-layout 'VkDescriptorSetLayout 0)
				:device device :allocator allocator))))
	(foreign-free p-bindings)))))

(defun destroy-descriptor-set-layout (dsl)
  (with-slots (device allocator) dsl
    (vkDestroyDescriptorSetLayout (h device) (h dsl) (h allocator))))
