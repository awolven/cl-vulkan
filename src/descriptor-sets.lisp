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
			      &key descriptor-buffer-info
				descriptor-image-info)
  (create-descriptor-set-1 device descriptor-set-layouts descriptor-pool
			   (append descriptor-buffer-info descriptor-image-info)))

(defun create-descriptor-set-1 (device descriptor-set-layouts descriptor-pool descriptor-infos)
  ;; at some point make-descriptor-sets a frame resource, may work for now
  (let ((descriptor-set
	 (allocate-descriptor-set device descriptor-set-layouts descriptor-pool))
	(count (length descriptor-infos))
	(free-list))
    (with-foreign-object (p-writes '(:struct VkWriteDescriptorSet) count)
      (loop for i from 0 below count
	 do (zero-struct (mem-aptr p-writes '(:struct VkWriteDescriptorSet) i)
			 '(:struct VkWriteDescriptorSet)))
      (flet ((alloc-image-info (info)
	       (let ((p-info (foreign-alloc '(:struct VkDescriptorImageInfo))))
		 (with-foreign-slots ((%vk::imageLayout
				       %vk::imageView
				       %vk::sampler)
				      p-info
				      (:struct VkDescriptorImageInfo))
		   (setf %vk::imageLayout VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
			 %vk::imageView (h (slot-value info 'image-view))
			 %vk::sampler (h (slot-value info 'sampler)))
		   (push p-info free-list)
		   p-info)))
	     
	     (alloc-buffer-info (info)
	       (let ((p-info (foreign-alloc '(:struct VkDescriptorBufferInfo))))
		 (with-foreign-slots ((%vk::buffer
				       %vk::offset
				       %vk::range)
				      p-info
				      (:struct VkDescriptorBufferInfo))
		   (setf %vk::buffer (h (buffer info))
			 %vk::offset (offset info)
			 %vk::range (range info))
		   (push p-info free-list)
		   p-info))))

	(unwind-protect
	     (progn
	       (loop for i from 0 for info in descriptor-infos
		  do (let* ((type (etypecase info
				    (descriptor-buffer-info :buffer)
				    (descriptor-image-info :image)))
			    (p-info (case type
				      (:buffer (alloc-buffer-info info))
				      (:image (alloc-image-info info))))
			    (p-write (mem-aptr p-writes '(:struct VkWriteDescriptorSet) i)))
		       (with-foreign-slots ((%vk::sType
					     %vk::dstSet
					     %vk::dstBinding
					     %vk::dstArrayElement
					     %vk::descriptorType
					     %vk::descriptorCount
					     %vk::pBufferInfo
					     %vk::pImageInfo
					     %vk::pTexelBufferView)
					    p-write
					    (:struct VkWriteDescriptorSet))
			 (setf %vk::sType VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
			       %vk::dstSet (h descriptor-set)
			       %vk::dstBinding i
			       %vk::dstArrayElement 0
			       %vk::descriptorType (descriptor-type info)
			       %vk::descriptorCount 1
			       %vk::pBufferInfo (if (eq type :buffer)
						    p-info
						    +nullptr+)
			       %vk::pImageInfo (if (eq type :image)
						   p-info
						   +nullptr+)
			       %vk::pTexelBufferView +nullptr+))))
	       (vkUpdateDescriptorSets (h device) count p-writes 0 +nullptr+))
	  (mapcar #'foreign-free free-list))
	descriptor-set))))

(defun free-descriptor-sets (descriptor-sets descriptor-pool)
  (with-slots (device) descriptor-pool
    (let ((count (length descriptor-sets)))
      (with-foreign-object (p-descriptor-sets 'VkDescriptorSet count)
	(loop for ds in descriptor-sets for i from 0
	   do (setf (mem-aref p-descriptor-sets 'VkDescriptorSet i) (h ds))
	   finally (vkFreeDescriptorSets (h device) (h descriptor-pool) count p-descriptor-sets)))))
  (values))
