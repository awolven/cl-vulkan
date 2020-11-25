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

(defcallback resize-framebuffer-callback :void ((window :pointer) (w :int) (h :int))
  (resize-framebuffer (find-window window) w h))

(defun set-framebuffer-size-callback (window &optional (callback-name 'resize-framebuffer-callback))
  (glfwSetFramebufferSizeCallback (h window) (get-callback callback-name)))

(defun create-framebuffer (device render-pass swapchain index &key (allocator +null-allocator+))
  (with-foreign-object (p-attachments 'VkImageView 2)
    (setf (mem-aref p-attachments 'VkImageView 0) (h (elt (color-image-views swapchain) index))
	  (mem-aref p-attachments 'VkImageView 1) (h (depth-image-view swapchain)))
	 
    (with-vk-struct (p-info VkFramebufferCreateInfo)
      (with-foreign-slots ((%vk::renderPass
			    %vk::attachmentCount
			    %vk::pAttachments
			    %vk::width
			    %vk::height
			    %vk::layers)
			   p-info (:struct VkFramebufferCreateInfo))
	(setf %vk::renderPass (h render-pass)
	      %vk::attachmentCount 2
	      %vk::pAttachments p-attachments
	      %vk::width (fb-width swapchain)
	      %vk::height (fb-height swapchain)
	      %vk::layers 1)
		
	(with-foreign-object (p-framebuffer 'VkFramebuffer)
	  (check-vk-result
	   (vkCreateFramebuffer (h device) p-info (h allocator) p-framebuffer))
	  (make-instance 'framebuffer :handle (mem-aref p-framebuffer 'VkFramebuffer)
			 :device device :allocator allocator))))))

(defun setup-framebuffers (device render-pass swapchain &key (allocator +null-allocator+))
  (let* ((count (length (images swapchain)))
	 (array (make-array count)))
    (loop for i from 0 below count
       do (setf (elt array i) (create-framebuffer device render-pass swapchain i :allocator allocator))
       finally (setf (framebuffers swapchain) array)))
  (values))

(defun resize-framebuffer (window width height)
  ;; imgui docking branch just sets a flag and does actual resize buffer in the
  ;; beginning of the render loop.  Could avoid certain errors.  Consider changing.
  (setf (recreate-swapchain? window) t
	(new-width window) width
	(new-height window) height)
  (values))

(defun destroy-framebuffers (swapchain)
  (with-slots (device) swapchain
    (with-slots (allocator) device
    (let ((framebuffers (framebuffers swapchain))
	  (device-handle (h device))
	  (allocator-handle (h allocator)))
      (loop for framebuffer across framebuffers
	 do (vkDestroyFramebuffer device-handle (h framebuffer) allocator-handle)
	 finally (setf (framebuffers swapchain) nil)))))
  (values))

(defun destroy-framebuffer (framebuffer)
  (vkDestroyFramebuffer (h (device framebuffer)) (h framebuffer) (h (allocator framebuffer)))
  (values))
