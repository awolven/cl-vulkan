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

(defun create-pipeline-layout (device descriptor-set-layouts &key (allocator +null-allocator+)
							                                   (push-constant-ranges nil))
  (with-pipeline-layout-create-info (p-create-info)
    (let ((dsl-count (length descriptor-set-layouts))
	      (push-constant-range-count (length push-constant-ranges)))
      (with-foreign-object (p-set-layouts 'VkDescriptorSetLayout dsl-count)
	    (loop for dsl in descriptor-set-layouts for i from 0
	          do (setf (mem-aref p-set-layouts 'VkDescriptorSetLayout i) (h dsl)))
	    (with-foreign-object (p-push-constant-ranges '(:struct VkPushConstantRange) push-constant-range-count)
	      (loop for pcr in push-constant-ranges for i from 0
	            do (with-foreign-slots ((%vk::stageFlags
				                         %vk::offset
				                         %vk::size)
				                        (mem-aptr p-push-constant-ranges '(:struct VkPushConstantRange) i)
				                        (:struct VkPushConstantRange))
		             (setf %vk::stageFlags (push-constant-range-stage-flags pcr)
			               %vk::offset (push-constant-range-offset pcr)
			               %vk::size (push-constant-range-size pcr))))
	      (with-foreign-slots ((%vk::sType
                                %vk::pNext
                                %vk::setLayoutCount
				                %vk::pSetLayouts
				                %vk::pushConstantRangeCount
				                %vk::pPushConstantRanges)
			                   p-create-info
			                   (:struct VkPipelineLayoutCreateInfo))
	        (setf %vk::sType VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
                  %vk::pNext +nullptr+
                  %vk::setLayoutCount dsl-count
		          %vk::pSetLayouts p-set-layouts
		          %vk::pushConstantRangeCount push-constant-range-count
		          %vk::pPushConstantRanges (if push-constant-ranges p-push-constant-ranges +nullptr+)))
	      (with-foreign-object (p-pipeline-layout 'VkPipelineLayout)
	        (vkCreatePipelineLayout (h device) p-create-info (h allocator) p-pipeline-layout)
	        (let ((pipeline-layout
		            (make-instance 'pipeline-layout :handle (mem-aref p-pipeline-layout 'VkPipelineLayout)
				                                    :device device :allocator allocator)))
	          (loop for dsl in descriptor-set-layouts
		            do (vector-push-extend dsl (descriptor-set-layouts pipeline-layout)))
	          pipeline-layout)))))))

(defun destroy-pipeline-layout (pipeline-layout)
  (with-slots (device allocator) pipeline-layout
    (vkDestroyPipelineLayout (h device) (h pipeline-layout) (h allocator)))
  (values))
