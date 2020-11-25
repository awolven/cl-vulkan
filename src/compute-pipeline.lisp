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

(defun create-compute-pipeline (device pipeline-layout shader-module
				&key
				  (pipeline-cache +null-pipeline-cache+)
				  (create-infos
				   (list (make-instance 'compute-pipeline-create-info
							:stage (make-instance 'shader-stage-create-info
									      :module shader-module)
							:layout pipeline-layout)))
				  (allocator +null-allocator+))

  (let ((create-info-count (length create-infos)))
    (with-foreign-object (p-create-infos '(:struct VkComputePipelineCreateInfo) create-info-count)
      (loop for ci in create-infos for i from 0
	 do (let ((p-create-info (mem-aptr p-create-infos '(:struct VkComputePipelineCreateInfo) i)))
	      (zero-struct p-create-info '(:struct VkComputePipelineCreateInfo))
	      (with-vk-struct (p-ssci VkPipelineShaderStageCreateInfo)
		(let ((stage (slot-value ci 'stage)))
		  (fill-pipeline-shader-stage-create-info
		   p-ssci
		   :stage (slot-value stage 'stage)
		   :module (slot-value stage 'module)
		   :p-name (foreign-string-alloc (slot-value stage 'name)))) ;;memory leak

		(with-foreign-slots ((%vk::sType
				      %vk::pNext
				      %vk::flags
				      %vk::stage
				      %vk::layout
				      %vk::basePipelineHandle
				      %vk::basePipelineIndex)
				     p-create-info (:struct VkComputePipelineCreateInfo))
		
		  (setf %vk::sType VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO
			%vk::pNext +nullptr+
			%vk::flags (slot-value ci 'flags)
			%vk::stage p-ssci
			%vk::layout (h (slot-value ci 'layout))
			%vk::basePipelineHandle (if (null-pointer-p (slot-value ci 'base-pipeline))
						   (slot-value ci 'base-pipeline)
						   (h (slot-value ci 'base-pipeline)))
			%vk::basePipelineIndex (slot-value ci 'base-pipeline-index))))))

      (with-foreign-object (p-pipelines 'VkPipeline 1)
	(check-vk-result
	 (vkCreateComputePipelines (h device) (h pipeline-cache)
				   create-info-count p-create-infos
				   (h allocator) p-pipelines))
	(make-instance 'compute-pipeline :handle (mem-aref p-pipelines 'VkPipeline)
		       :device device
		       :allocator allocator)))))
