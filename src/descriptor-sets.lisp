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

(defun allocate-descriptor-set (device descriptor-set-layouts descriptor-pool)
  (let ((dsl-count (length descriptor-set-layouts)))
    (with-foreign-object (p-descriptor-set-layouts 'VkDescriptorSetLayout dsl-count)
      (loop for dsl in descriptor-set-layouts for i from 0
	 do (setf (mem-aref p-descriptor-set-layouts 'VkDescriptorSetLayout i) (h dsl)))
      (with-vk-struct (p-alloc-info VkDescriptorSetAllocateInfo)
	(with-foreign-slots ((%vk::descriptorPool
			      %vk::descriptorSetCount
			      %vk::pSetLayouts)
			     p-alloc-info (:struct VkDescriptorSetAllocateInfo))
	  (setf %vk::descriptorPool (h descriptor-pool)
		%vk::descriptorSetCount dsl-count
		%vk::pSetLayouts p-descriptor-set-layouts)
	  
	  (with-foreign-object (p-descriptor-set 'VkDescriptorSet)
	    (check-vk-result (vkAllocateDescriptorSets (h device) p-alloc-info p-descriptor-set))
	    (make-instance 'descriptor-set
			   :handle (mem-aref p-descriptor-set 'VkDescriptorSet)
			   :device device
			   :descriptor-pool descriptor-pool)))))))

(defun create-descriptor-set (device descriptor-set-layouts descriptor-pool
			      &key descriptor-buffer-info)

  ;; this function will probably prove to be very over-generalized.

  (let* ((descriptor-set
	  (allocate-descriptor-set device descriptor-set-layouts descriptor-pool))
	 (count (length descriptor-buffer-info))
	 (p-buffer-infos (foreign-alloc '(:struct VkDescriptorBufferInfo) :count count))
	 (p-descriptor-writes (foreign-alloc '(:struct VkWriteDescriptorSet) :count count)))
    
    (unwind-protect
	 (progn
	   (loop for info in descriptor-buffer-info for i from 0
	      do (let ((p-buffer-info (mem-aptr p-buffer-infos '(:struct VkDescriptorBufferInfo) i)))
		   (zero-struct p-buffer-info '(:struct VkDescriptorBufferInfo))
		   (with-foreign-slots ((%vk::buffer
					 %vk::offset
					 %vk::range)
					p-buffer-info
					(:struct VkDescriptorBufferInfo))
		     
		     (setf %vk::buffer (h (buffer info))
			   %vk::offset (offset info)
			   %vk::range (range info)))
	   
		   (let ((p-descriptor-write
			  (mem-aptr p-descriptor-writes '(:struct VkWriteDescriptorSet) i)))
		     (zero-struct p-descriptor-write '(:struct VkWriteDescriptorSet))
		     (with-foreign-slots ((%vk::sType
					   %vk::dstSet
					   %vk::dstBinding
					   %vk::dstArrayElement
					   %vk::descriptorType
					   %vk::descriptorCount
					   %vk::pBufferInfo
					   %vk::pImageInfo
					   %vk::pTexelBufferView)
					  p-descriptor-write
					  (:struct VkWriteDescriptorSet))
		       (setf %vk::sType VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
			     %vk::dstSet (h descriptor-set)
			     %vk::dstBinding i
			     %vk::dstArrayElement 0
			     %vk::descriptorType (descriptor-type info)
			     %vk::descriptorCount 1
			     %vk::pBufferInfo p-buffer-info
			     %vk::pImageInfo +nullptr+
			     %vk::pTexelBufferView +nullptr+)))))
	   (vkUpdateDescriptorSets (h device) count p-descriptor-writes 0 +nullptr+))
      (foreign-free p-buffer-infos)
      (foreign-free p-descriptor-writes))
    
    descriptor-set))

(defun free-descriptor-sets (descriptor-sets descriptor-pool)
  (with-slots (device) descriptor-pool
    (let ((count (length descriptor-sets)))
      (with-foreign-object (p-descriptor-sets 'VkDescriptorSet count)
	(loop for ds in descriptor-sets for i from 0
	   do (setf (mem-aref p-descriptor-sets 'VkDescriptorSet i) (h ds))
	   finally (vkFreeDescriptorSets (h device) (h descriptor-pool) count p-descriptor-sets)))))
  (values))
