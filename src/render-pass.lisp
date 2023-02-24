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

(defun create-render-pass (device format-enum &key (allocator +null-allocator+)

						   (color-attachments (list (make-instance 'color-attachment
											   :name :default-color-attachment
											   :format format-enum)))
						   (depth-attachments (list (make-instance 'depth-attachment
											   :name :default-depth-stencil-attachment
											   :format (find-supported-depth-format (physical-device device)))))
						   (subpasses (list (make-instance 'subpass
										   :name :default-subpass
										   :color-attachments (list :default-color-attachment)
										   :depth-stencil-attachment :default-depth-stencil-attachment))))
										   
  (let ((attachment-count (+ (length color-attachments) (length depth-attachments)))
	(pointers ()))
    (with-foreign-object (p-attachments '(:struct VkAttachmentDescription) attachment-count) ;; todo: make with-vk-struct take a count.  Long overdue.
      (loop for i from 0 for attachment in (append color-attachments depth-attachments) 
	 do (zero-struct (mem-aptr p-attachments '(:struct VkAttachmentDescription) i) '(:struct VkAttachmentDescription))
	   (with-foreign-slots ((%vk::format
				 %vk::samples
				 %vk::loadOp
				 %vk::storeOp
				 %vk::stencilLoadOp
				 %vk::stencilStoreOp
				 %vk::initialLayout
				 %vk::finalLayout)
				(mem-aptr p-attachments '(:struct VkAttachmentDescription) i) (:struct VkAttachmentDescription))
	     (setf %vk::format (attachment-format attachment)
		   %vk::samples (samples attachment)
		   %vk::loadOp (load-op attachment)
		   %vk::storeOp (store-op attachment)
		   %vk::stencilLoadOp (stencil-load-op attachment)
		   %vk::stencilStoreOp (stencil-store-op attachment)
		   %vk::initialLayout (initial-layout attachment)
		   %vk::finalLayout (final-layout attachment))))

      (let ((subpass-count (length subpasses)))

	(unwind-protect
	     (with-foreign-object (p-subpasses '(:struct VkSubpassDescription) subpass-count)
	       (loop for i from 0 for subpass in subpasses
		  do (zero-struct (mem-aptr p-subpasses '(:struct VkSubpassDescription) i) '(:struct VkSubpassDescription))
	       
		    (with-foreign-slots ((%vk::pipelineBindPoint
					  %vk::colorAttachmentCount
					  %vk::pColorAttachments
					  %vk::pDepthStencilAttachment)
					 (mem-aptr p-subpasses '(:struct VkSubpassDescription) i)
					 (:struct VkSubpassDescription))
		      (let* ((color-attachment-references (color-attachments subpass))
			     (reference-count (length color-attachment-references)))
			(let ((p-attachment-refs (foreign-alloc '(:struct VkAttachmentReference) :count reference-count)))
			  (push p-attachment-refs pointers)
			  (loop for reference in color-attachment-references for i from 0
			     do (%vk::zero-struct (mem-aptr p-attachment-refs '(:struct VkAttachmentReference) i) '(:struct VkAttachmentReference))
			       (with-foreign-slots ((%vk::attachment %vk::layout)
						    (mem-aptr p-attachment-refs '(:struct VkAttachmentReference) i)
						    (:struct VkAttachmentReference))
				 (setf %vk::attachment (position reference color-attachments :key #'attachment-name)
				       %vk::layout (reference-layout (find reference color-attachments :key #'attachment-name)))))
			  (let ((p-depth-attachment-ref (foreign-alloc '(:struct VkAttachmentReference))))
			    (zero-struct p-depth-attachment-ref '(:struct VkAttachmentReference))
			    (push p-depth-attachment-ref pointers)
			    (with-foreign-slots ((%vk::attachment %vk::layout)
						 p-depth-attachment-ref
						 (:struct VkAttachmentReference))
			      (setf %vk::attachment (+ (length color-attachments) (position :default-depth-stencil-attachment depth-attachments :key #'attachment-name))
				    %vk::layout (reference-layout (find :default-depth-stencil-attachment depth-attachments :key #'attachment-name))))
		       
			    (setf %vk::pipelineBindPoint (pipeline-bind-point subpass)
				  %vk::colorAttachmentCount reference-count
				  %vk::pColorAttachments p-attachment-refs
				  %vk::pDepthStencilAttachment p-depth-attachment-ref))))))
	
	       (with-vk-struct (p-info VkRenderPassCreateInfo)
		 (with-foreign-slots ((%vk::attachmentCount
				       %vk::pAttachments
				       %vk::subpassCount
				       %vk::pSubpasses
				       %vk::dependencyCount
				       %vk::pDependencies)
				      p-info (:struct VkRenderPassCreateInfo))
		   (setf %vk::attachmentCount attachment-count
			 %vk::pAttachments p-attachments
			 %vk::subpassCount subpass-count
			 %vk::pSubpasses p-subpasses
			 %vk::dependencyCount 0
			 %vk::pDependencies +nullptr+))
		
		 (with-foreign-object (p-render-pass 'VkRenderPass)
		   (check-vk-result (vkCreateRenderPass (h device) p-info (h allocator) p-render-pass))
		   (make-instance 'render-pass :handle (mem-aref p-render-pass 'VkRenderPass)
				  :device device :allocator allocator))))
	  (mapcar #'foreign-free pointers))))))

(defun destroy-render-pass (render-pass)
  (with-slots (device) render-pass
    (with-slots (allocator) device
      (vkDestroyRenderPass (h device) (h render-pass) (h allocator))))
  (values))
