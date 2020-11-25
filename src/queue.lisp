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

(defun queue-family (queue)
  (nth (queue-family-index queue) (queue-families (physical-device (device queue)))))

(defun find-queue (device queue-family-index)
  (let ((entry (assoc queue-family-index (device-queues device))))
    (if entry
	(first (second entry))
	(error "Could not find device queue fo device ~S of queue-family-index ~A" device queue-family-index))))

(defun queue-wait-idle (queue)
  (check-vk-result (vkQueueWaitIdle (h queue))))

(defun queue-submit1 (queue command-buffer)
  (with-foreign-objects ((p-command-buffer 'VkCommandBuffer))
    (setf (mem-aref p-command-buffer 'VkCommandBuffer) (h command-buffer))
    (with-vk-struct (p-end-info VkSubmitInfo)
      (with-foreign-slots ((%vk::commandBufferCount %vk::pCommandBuffers)
			   p-end-info (:struct VkSubmitInfo))
	(setf %vk::commandBufferCount 1
	      %vk::pCommandBuffers p-command-buffer)

	(check-vk-result (vkQueueSubmit (h queue) 1 p-end-info VK_NULL_HANDLE))))))

(defun queue-submit (queue command-buffer wait-semaphore signal-semaphore fence
		     &optional (wait-stage-mask VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT))
  (with-foreign-object (p-command-buffer 'VkCommandBuffer)
    (setf (mem-aref p-command-buffer 'VkCommandBuffer) (h command-buffer))
    (with-foreign-object (p-wait-semaphore 'VkSemaphore)
      (setf (mem-aref p-wait-semaphore 'VkSemaphore) (h wait-semaphore))
      (with-foreign-object (p-signal-semaphore 'VkSemaphore)
	(setf (mem-aref p-signal-semaphore 'VkSemaphore) (h signal-semaphore))
	(with-vk-struct (p-submit-info VkSubmitInfo)
	  (with-foreign-object (p-wait-stage 'VkPipelineStageFlags)
	    (setf (mem-aref p-wait-stage 'VkPipelineStageFlags) wait-stage-mask)
	    (with-foreign-slots ((%vk::commandBufferCount
				  %vk::pCommandBuffers
				  %vk::waitSemaphoreCount
				  %vk::pWaitSemaphores
				  %vk::pWaitDstStageMask
				  %vk::signalSemaphoreCount
				  %vk::pSignalSemaphores)
				 p-submit-info (:struct VkSubmitInfo))
	      (setf %vk::commandBufferCount 1
		    %vk::pCommandBuffers p-command-buffer
		    %vk::waitSemaphoreCount 1
		    %vk::pWaitSemaphores p-wait-semaphore
		    %vk::pWaitDstStageMask p-wait-stage
		    %vk::signalSemaphoreCount 1
		    %vk::pSignalSemaphores p-signal-semaphore)

	      (check-vk-result
	       (vkQueueSubmit (h queue) 1 p-submit-info (h fence))))))))))
