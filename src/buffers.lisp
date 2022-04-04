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

(defun create-buffer-1 (device size usage &key (allocator +null-allocator+)
					    (buffer-class 'buffer))
  (with-vk-struct (p-info VkBufferCreateInfo)
    (with-foreign-slots ((%vk::size
			  %vk::usage
			  %vk::sharingMode)
			 p-info (:struct VkBufferCreateInfo))
      (setf %vk::size size
	    %vk::usage usage
	    %vk::sharingMode VK_SHARING_MODE_EXCLUSIVE)
      (with-foreign-object (p-buffer 'VkBuffer)
	(check-vk-result (vkCreateBuffer (h device) p-info (h allocator) p-buffer))
	(make-instance buffer-class :handle (mem-aref p-buffer 'VkBuffer)
		       :size size :device device :allocator allocator)))))

(defun destroy-buffer (buffer)
  (with-slots (device) buffer
    (vkDestroyBuffer (h device) (h buffer) (h (allocator buffer)))
    (vkFreeMemory (h device) (h (allocated-memory buffer)) (h (allocator (allocated-memory buffer)))))
  (values))

(defun allocate-buffer-memory (device buffer properties &key (allocator +null-allocator+))
  (with-vk-struct (p-requirements VkMemoryRequirements)
    (vkGetBufferMemoryRequirements (h device) (h buffer) p-requirements)
    
    (with-vk-struct (p-alloc-info VkMemoryAllocateInfo)
      (with-foreign-slots ((%vk::allocationSize
			                %vk::memoryTypeIndex)
			               p-alloc-info
			               (:struct VkMemoryAllocateInfo))
	    (setf %vk::allocationSize
	          (foreign-slot-value p-requirements '(:struct VkMemoryRequirements) '%vk::size)
	          %vk::memoryTypeIndex
	          (find-memory-type
	           (physical-device device)
	           (foreign-slot-value p-requirements '(:struct VkMemoryRequirements) '%vk::memoryTypeBits)
	           properties)))
      (with-foreign-object (p-buffer-memory 'VkDeviceMemory)
	    (check-vk-result (vkAllocateMemory (h device) p-alloc-info (h allocator) p-buffer-memory))
	    (make-instance 'allocated-memory :handle (mem-aref p-buffer-memory 'VkDeviceMemory)
		                                 :device device
		                                 :allocator allocator
		                                 :alignment (foreign-slot-value p-requirements
						                                                '(:struct VkMemoryRequirements)
						                                                '%vk::alignment))))))

(defun bind-buffer-memory (device buffer buffer-memory &optional (offset 0))
  (vkBindBufferMemory (h device) (h buffer) (h buffer-memory) offset))

(defun bind-buffer-memory-resource (device buffer memory-resource &optional (offset 0))
  (vkBindBufferMemory (h device) (h buffer)
                      (h (vk::allocation
                          (vk::memory-resource-memory-pool memory-resource)))
                      offset))

(defun copy-buffer (device command-pool queue src-buffer dst-buffer size)
  (with-vk-struct (p-alloc-info VkCommandBufferAllocateInfo)
    (with-foreign-slots ((%vk::level
			  %vk::commandPool
			  %vk::commandBufferCount)
			 p-alloc-info (:struct VkCommandBufferAllocateInfo))
      (setf %vk::level VK_COMMAND_BUFFER_LEVEL_PRIMARY
	    %vk::commandPool (h command-pool)
	    %vk::commandBufferCount 1)

      (with-foreign-object (p-command-buffer 'VkCommandBuffer)
	(vkAllocateCommandBuffers (h device) p-alloc-info p-command-buffer)
	(let ((command-buffer (make-instance 'command-buffer :handle (mem-aref p-command-buffer 'VkCommandBuffer)
					     :device device :command-pool command-pool)))
	  (with-vk-struct (p-begin-info VkCommandBufferBeginInfo)
	    (with-foreign-slots ((%vk::flags)
				 p-begin-info (:struct VkCommandBufferBeginInfo))
	      (setf %vk::flags VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)
	      (vkBeginCommandBuffer (h command-buffer) p-begin-info)
	      (with-vk-struct (p-copy-region VkBufferCopy)
		(with-foreign-slots ((%vk::srcOffset
				      %vk::dstOffset
				      %vk::size)
				     p-copy-region (:struct VkBufferCopy))
		  (setf %vk::srcOffset 0
			%vk::dstOffset 0
			%vk::size size)
		  (vkCmdCopyBuffer (h command-buffer) (h src-buffer) (h dst-buffer) 1 p-copy-region)
		  (vkEndCommandBuffer (h command-buffer))
		  (queue-submit1 queue command-buffer)
		  (vkQueueWaitIdle (h queue))
		  (vkFreeCommandBuffers (h device) (h command-pool) 1 p-command-buffer)))))))))
  (values))

(defun create-empty-buffer (device size usage &key (allocator +null-allocator+)
						(memory-properties
						 (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
							 VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))
						(buffer-class 'buffer))
  (let* ((buffer (create-buffer-1 device size usage :buffer-class buffer-class :allocator allocator))
	 (buffer-memory (allocate-buffer-memory device buffer memory-properties
						:allocator allocator)))
    (bind-buffer-memory device buffer buffer-memory)
    (setf (allocated-memory buffer) buffer-memory)
    buffer))

(defun create-buffer (device data size usage &key (allocator +null-allocator+)
					       (memory-properties VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
					       (buffer-class 'buffer))
  (let* ((staging-buffer (create-buffer-1 device size VK_BUFFER_USAGE_TRANSFER_SRC_BIT :allocator allocator))
	 (staging-buffer-memory (allocate-buffer-memory device staging-buffer
						 (logior  VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
							  VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
						 :allocator allocator)))
    (bind-buffer-memory device staging-buffer staging-buffer-memory)
    (with-foreign-object (pp-data :pointer)
      (vkMapMemory (h device) (h staging-buffer-memory) 0 size 0 pp-data)
      (memcpy (mem-aref pp-data :pointer) data size)
      (vkUnmapMemory (h device) (h staging-buffer-memory)))

    (let* ((buffer (create-buffer-1 device size (logior VK_BUFFER_USAGE_TRANSFER_DST_BIT usage)
				    :buffer-class buffer-class :allocator allocator))
	   (buffer-memory (allocate-buffer-memory device buffer memory-properties
						  :allocator allocator))
	   (queue-family-index (get-any-queue-family-index-with-transfer-support (physical-device device)))
	   (queue (find-queue device queue-family-index))
	   (command-pool (find-command-pool device queue-family-index)))
      (bind-buffer-memory device buffer buffer-memory)
      (copy-buffer device command-pool queue staging-buffer buffer size)
      (vkDestroyBuffer (h device) (h staging-buffer) (h allocator))
      (vkFreeMemory (h device) (h staging-buffer-memory) (h allocator))
      (setf (allocated-memory buffer) buffer-memory)
      buffer)))

(defun create-vertex-buffer (device data size &key (allocator +null-allocator+))
  (create-buffer device data size VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
		 :buffer-class 'vertex-buffer :allocator allocator))

(defun create-index-buffer (device data size &key (allocator +null-allocator+))
  (create-buffer device data size VK_BUFFER_USAGE_INDEX_BUFFER_BIT
		 :buffer-class 'index-buffer :allocator allocator))

(defun create-uniform-buffer (device size &key (allocator +null-allocator+))
  (let* ((buffer (create-buffer-1 device size VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
				  :buffer-class 'uniform-buffer :allocator allocator))
	 (memory (allocate-buffer-memory device buffer (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
							       VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
				  :allocator allocator)))
    (bind-buffer-memory device buffer memory)
    (setf (allocated-memory buffer) memory)
    buffer))

(defun copy-uniform-buffer-memory (device data uniform-buffer-memory size)
  (with-foreign-object (pp-data :pointer)
    (vkMapMemory (h device) (h uniform-buffer-memory) 0 size 0 pp-data)
    (memcpy (mem-aref pp-data :pointer) data size)
    (vkUnmapMemory (h device) (h uniform-buffer-memory))))

(defun mmap-buffer (buffer array size)
  (let ((memory (allocated-memory buffer))
	(device (device buffer)))
    (with-foreign-object (pp-dst :pointer)
		    
      (check-vk-result (vkMapMemory (h device) (h memory) 0 size 0 pp-dst))
		  
      (let ((p-dst (mem-aref pp-dst :pointer)))
	(memcpy p-dst array size)
		    
	(with-foreign-object (p-range '(:struct VkMappedMemoryRange))
	  (zero-struct p-range '(:struct VkMappedMemoryRange))
		      
	  (with-foreign-slots ((%vk::sType
				%vk::memory
				%vk::size)
			       p-range (:struct VkMappedMemoryRange))
	    
	    (setf %vk::sType  VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
		  %vk::memory (h memory)
		  %vk::size VK_WHOLE_SIZE))
	  
	  (check-vk-result (vkFlushMappedMemoryRanges (h device) 1 p-range))

	  (vkUnmapMemory (h device) (h memory))
	  
	  (values))))))
