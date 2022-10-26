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

(defun create-command-buffer (device command-pool &key (allocator +null-allocator+))
  (let ((command-buffer (create-command-buffer-1 device command-pool :allocator allocator))
	(command-buffers (command-buffers command-pool)))
    ;; store command buffers outside of frame-resources as well for non-frame-related command-buffer use
    (vector-push-extend command-buffer command-buffers)
    command-buffer))

(defun create-command-buffer-1 (device command-pool &key (allocator +null-allocator+))
  (with-vk-struct (p-info VkCommandBufferAllocateInfo)
    (with-foreign-slots ((%vk::commandPool
			  %vk::level
			  %vk::commandBufferCount)
			 p-info
			 (:struct VkCommandBufferAllocateInfo))
      (setf %vk::commandPool (h command-pool)
	    %vk::level VK_COMMAND_BUFFER_LEVEL_PRIMARY
	    %vk::commandBufferCount 1)
      (with-foreign-object (p-command-buffer 'VkCommandBuffer)
	(check-vk-result (vkAllocateCommandBuffers (h device) p-info p-command-buffer))
	(make-instance 'command-buffer
		       :handle (mem-aref p-command-buffer 'VkCommandBuffer)
		       :device device :command-pool command-pool
		       :allocator allocator)))))

(defun free-command-buffers (command-pool)
  (let* ((command-buffers (command-buffers command-pool))
	 (count (length command-buffers)))
    (with-slots (device) command-pool
      (with-foreign-object (p-command-buffers 'VkCommandBuffer count)
	(loop for command-buffer across command-buffers for i from 0
	   do (setf (mem-aref p-command-buffers 'VkCommandBuffer i) (h command-buffer)))
	(vkFreeCommandBuffers (h device) (h command-pool) count p-command-buffers)
	(setf (fill-pointer command-buffers) 0)))))

(defun free-command-buffer (command-buffer)
  (let ((command-pool (command-pool command-buffer)))
    (with-foreign-object (p-command-buffer 'VkCommandBuffer)
      (setf (mem-aref p-command-buffer 'VkCommandBuffer) (h command-buffer))
      (vkFreeCommandBuffers (h (device command-pool)) (h command-pool) 1 p-command-buffer)))
  (values))

(defun begin-command-buffer (command-buffer)
  (with-vk-struct (p-begin-info VkCommandBufferBeginInfo)
    (with-foreign-slots ((%vk::flags)
			 p-begin-info (:struct VkCommandBufferBeginInfo))
      (setf %vk::flags (logior %vk::flags VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT))
      
      (check-vk-result
       (vkBeginCommandBuffer (h command-buffer) p-begin-info)))))

(defun end-command-buffer (command-buffer)
  (check-vk-result (vkEndCommandBuffer (h command-buffer))))

(defun cmd-set-viewport (command-buffer &key (x 0.0f0) (y 0.0f0) width height (min-depth 0.0f0) (max-depth 1.0f0))
  (with-viewport (p-viewport :x x :y y :width width :height height :min-depth min-depth :max-depth max-depth)
    (vkCmdSetViewport (h command-buffer) 0 1 p-viewport)))

(defun cmd-set-scissor (command-buffer &key (x 0) (y 0) width height)
  (with-scissor (p-scissor :x x :y y :width width :height height)
    (vkCmdSetScissor (h command-buffer) 0 1 p-scissor)))

(defun cmd-bind-pipeline (command-buffer pipeline &key (bind-point :graphics))
  (vkCmdBindPipeline (h command-buffer)
		     (ecase bind-point
		       (:graphics VK_PIPELINE_BIND_POINT_GRAPHICS)
		       (:compute VK_PIPELINE_BIND_POINT_COMPUTE))
		     (h pipeline)))

(defun cmd-bind-vertex-buffers (command-buffer vertex-buffers &optional (buffer-offsets (list 0))
								(first-binding 0)
								(binding-count 1))
  (let ((number-buffers (length vertex-buffers))
	(number-offsets (length buffer-offsets)))
    (assert (or (eq number-buffers number-offsets) (eq number-offsets 1)))
    (when (and (> number-buffers 1) (eq number-offsets 1))
      (setq buffer-offsets (make-list number-buffers :initial-element (first buffer-offsets))))
    (with-foreign-objects ((p-vertex-buffers 'VkBuffer number-buffers)
			   (p-offsets 'VkDeviceSize number-buffers))
      (loop for i from 0 below number-buffers for buffer in vertex-buffers for offset in buffer-offsets
	 do (setf (mem-aref p-vertex-buffers 'VkBuffer i) (h buffer)
		  (mem-aref p-offsets 'VkDeviceSize) offset))
      (vkCmdBindVertexBuffers (h command-buffer) first-binding binding-count p-vertex-buffers p-offsets))))

(defun cmd-bind-descriptor-sets (command-buffer pipeline-layout descriptor-sets &optional (bind-point :graphics))
  (let ((count (length descriptor-sets)))
    (with-foreign-object (p-descriptor-sets 'VkDescriptorSet count)
      (loop for ds in descriptor-sets
	 do
	   (setf (mem-aref p-descriptor-sets 'VkDescriptorSet) (h ds)))
      (vkCmdBindDescriptorSets (h command-buffer)
			       (ecase bind-point
				 (:graphics VK_PIPELINE_BIND_POINT_GRAPHICS)
				 (:compute VK_PIPELINE_BIND_POINT_COMPUTE))
			       (h pipeline-layout)
			       0 count p-descriptor-sets 0 +nullptr+))))

(defun cmd-bind-index-buffer (command-buffer index-buffer &optional (offset 0) (integer-type :unsigned-short))
  (vkCmdBindIndexBuffer (h command-buffer) (h index-buffer) offset (ecase integer-type
                                                                     (:unsigned-short VK_INDEX_TYPE_UINT16)
                                                                     (:unsigned-int VK_INDEX_TYPE_UINT32)
                                                                     (:unsigned-int32 VK_INDEX_TYPE_UINT32))))

(defun cmd-draw-indexed (command-buffer command)
  (vkCmdDrawIndexed (h command-buffer)
		    (draw-indexed-cmd-index-count command)
		    1
		    (draw-indexed-cmd-first-index command)
		    (draw-indexed-cmd-vertex-offset command)
		    0))
