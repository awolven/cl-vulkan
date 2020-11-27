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

(defun fill-vertex-input-binding-description (p
					      &key
						(input-binding-number 0)
						stride
						(input-rate VK_VERTEX_INPUT_RATE_VERTEX)
						&allow-other-keys)
  (with-foreign-slots ((%vk::binding
			%vk::stride
			%vk::inputRate)
		       p
		       (:struct VkVertexInputBindingDescription))
    (setf %vk::binding input-binding-number
	  %vk::stride stride
	  %vk::inputRate input-rate)))



(defun fill-vertex-input-attribute-description (p-attribute-description &key
									  (attribute-binding-number 0)
									  (location 0)
									  (format VK_FORMAT_R32G32B32_SFLOAT)
									  offset
									  &allow-other-keys)
  (zero-struct p-attribute-description '(:struct VkVertexInputAttributeDescription))
  (with-foreign-slots ((%vk::binding
			%vk::location
			%vk::format
			%vk::offset)
		       p-attribute-description
		       (:struct VkVertexInputAttributeDescription))
    (setf %vk::binding attribute-binding-number
	  %vk::location location
	  %vk::format format
	  %vk::offset offset))
  (values))									     

(defun fill-pipeline-vertex-input-state-create-info (p-ci
						     &key
						       (vertex-binding-description-count 1)
						       p-vertex-binding-descriptions
						       (vertex-attribute-description-count 2)
						       p-vertex-attribute-descriptions
						       &allow-other-keys)
  (with-foreign-slots ((%vk::vertexBindingDescriptionCount
			%vk::pVertexBindingDescriptions
			%vk::vertexAttributeDescriptionCount
			%vk::pVertexAttributeDescriptions)
		       p-ci
		       (:struct VkPipelineVertexInputStateCreateInfo))
    (setf %vk::vertexBindingDescriptionCount vertex-binding-description-count
	  %vk::pVertexBindingDescriptions p-vertex-binding-descriptions
	  %vk::vertexAttributeDescriptionCount vertex-attribute-description-count
	  %vk::pVertexAttributeDescriptions p-vertex-attribute-descriptions))
  (values))

  


(defun fill-pipeline-input-assembly-state-create-info (p-ci
						       &key
							 (topology VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST)
							 (primitive-restart-enable VK_FALSE)
							 &allow-other-keys)
  (with-foreign-slots ((%vk::topology
			%vk::primitiveRestartEnable)
		       p-ci
		       (:struct VkPipelineInputAssemblyStateCreateInfo))
    (setf %vk::topology topology
	  %vk::primitiveRestartEnable primitive-restart-enable))
  (values))

  


(defun fill-viewport-structure (p
				&key
				  (viewport-x 0.0f0)
				  (viewport-y 0.0f0)
				  (viewport-width 0.0f0)
				  (viewport-height 0.0f0)
				  (min-depth 0.0f0)
				  (max-depth 1.0f0)
				  &allow-other-keys)
  (with-foreign-slots ((%vk::x
			%vk::y
			%vk::width
			%vk::height
			%vk::minDepth
			%vk::maxDepth)
		       p (:struct VkViewport))
    (setf %vk::x viewport-x
	  %vk::y viewport-y
	  %vk::width viewport-width
	  %vk::height viewport-height
	  %vk::minDepth min-depth
	  %vk::maxDepth max-depth))
  (values))



(defun fill-scissor-structure (p
			       &key
				 (scissor-x 0)
				 (scissor-y 0)
				 (scissor-width 0)
				 (scissor-height 0)
				 &allow-other-keys)
  (setf (foreign-slot-value
	    (foreign-slot-pointer p '(:struct VkRect2D) '%vk::offset)
	    '(:struct VkOffset2D)
	    '%vk::x) scissor-x
				 
	    (foreign-slot-value
	     (foreign-slot-pointer p '(:struct VkRect2D) '%vk::offset)
	     '(:struct VkOffset2D)
	     '%vk::y) scissor-y
				  
	     (foreign-slot-value
	      (foreign-slot-pointer p '(:struct VkRect2D) '%vk::extent)
	      '(:struct VkExtent2D)
	      '%vk::width) scissor-width
				   
	      (foreign-slot-value
	       (foreign-slot-pointer p '(:struct VkRect2D) '%vk::extent)
	       '(:struct VkExtent2D)
	       '%vk::height) scissor-height)
  (values))
  


(defun fill-pipeline-viewport-state-create-info (p-ci
						 &key
						   (viewport-count 1)
						   p-viewports
						   (scissor-count 1)
						   p-scissors
						   &allow-other-keys)
  (with-foreign-slots ((%vk::viewportCount
			%vk::pViewports
			%vk::scissorCount
			%vk::pScissors)
		       p-ci
		       (:struct VkPipelineViewportStateCreateInfo))
    (setf %vk::viewportCount viewport-count
	  %vk::pViewports p-viewports
	  %vk::scissorCount scissor-count
	  %vk::pScissors p-scissors))
  (values))
  

(defun fill-pipeline-rasterization-line-state-create-info-ext
    (p-ci &key (line-rasterization-mode %vk::VK_LINE_RASTERIZATION_MODE_DEFAULT_EXT)
	    (stippled-line-enable VK_FALSE)
	    (line-stipple-factor 1)
	    (line-stipple-pattern #b1111111100000000)
     &allow-other-keys)
  
  (with-foreign-slots ((%vk::lineRasterizationMode
			%vk::stippledLineEnable
			%vk::lineStippleFactor
			%vk::lineStipplePattern)
		       p-ci
		       (:struct %vk::VkPipelineRasterizationLineStateCreateInfoEXT))
    (setf %vk::lineRasterizationMode line-rasterization-mode
	  %vk::stippledLineEnable stippled-line-enable
	  %vk::lineStippleFactor line-stipple-factor
	  %vk::lineStipplePattern line-stipple-pattern)
    (values)))

(defun fill-physical-device-line-rasterization-features-ext
    (p-features &key (rectangular-lines VK_FALSE)
		  (bresenham-lines VK_FALSE)
		  (smooth-lines VK_FALSE)
		  (stippled-rectangular-lines VK_FALSE)
		  (stippled-bresenham-lines VK_FALSE)
		  (stippled-smooth-lines VK_FALSE)
		  &allow-other-keys)
  
  (with-foreign-slots ((%vk::rectangularLines
			%vk::bresenhamLines
			%vk::smoothLines
			%vk::stippledRectangularLines
			%vk::stippledBresenhamLines
			%vk::stippledSmoothLines)
		       p-features
		       (:struct %vk::VkPhysicalDeviceLineRasterizationFeaturesEXT))
    (setf %vk::rectangularLines rectangular-lines
	  %vk::bresenhamLines bresenham-lines
	  %vk::smoothLines smooth-lines
	  %vk::stippledRectangularLines stippled-rectangular-lines
	  %vk::stippledBresenhamLines stippled-bresenham-lines
	  %vk::stippledSmoothLines stippled-smooth-lines)
    (values)))		      

(defun fill-pipeline-rasterization-state-create-info (p-ci
						      &key
							(depth-clamp-enable VK_TRUE)
							(rasterizer-discard-enable VK_FALSE)
							(polygon-mode VK_POLYGON_MODE_FILL)
							(line-width 1.0f0)
							(cull-mode VK_CULL_MODE_NONE)
							(front-face VK_FRONT_FACE_COUNTER_CLOCKWISE)
							(depth-bias-enable VK_FALSE)
							(depth-bias-constant-factor 0.0f0)
							(depth-bias-clamp 0.0f0)
							(depth-bias-slope-factor 0.0f0)
							&allow-other-keys)
  (with-foreign-slots ((%vk::depthClampEnable
			%vk::rasterizerDiscardEnable
			%vk::polygonMode
			%vk::lineWidth
			%vk::cullMode
			%vk::frontFace
			%vk::depthBiasEnable
			%vk::depthBiasConstantFactor
			%vk::depthBiasClamp
			%vk::depthBiasSlopeFactor)
		       p-ci
		       (:struct VkPipelineRasterizationStateCreateInfo))
    (setf %vk::depthClampEnable depth-clamp-enable
	  %vk::rasterizerDiscardEnable rasterizer-discard-enable
	  %vk::polygonMode polygon-mode
	  %vk::lineWidth line-width
	  %vk::cullMode cull-mode
	  %vk::frontFace front-face
	  %vk::depthBiasEnable depth-bias-enable
	  %vk::depthBiasConstantFactor depth-bias-constant-factor
	  %vk::depthBiasClamp depth-bias-clamp
	  %vk::depthBiasSlopeFactor depth-bias-slope-factor))
  (values))
  


(defun fill-pipeline-multisample-state-create-info (p-ci
						    &key
						      (sample-shading-enable VK_FALSE)
						      (rasterization-samples VK_SAMPLE_COUNT_1_BIT)
						      (min-sample-shading 1.0f0)
						      (p-sample-mask +nullptr+)
						      (alpha-to-coverage-enable VK_FALSE)
						      (alpha-to-one-enable VK_FALSE)
						      &allow-other-keys)
  (with-foreign-slots ((%vk::sampleShadingEnable
			%vk::rasterizationSamples
			%vk::minSampleShading
			%vk::pSampleMask
			%vk::alphaToCoverageEnable
			%vk::alphaToOneEnable)
		       p-ci
		       (:struct VKPipelineMultisampleStateCreateInfo))
    (setf %vk::sampleShadingEnable sample-shading-enable
	  %vk::rasterizationSamples rasterization-samples
	  %vk::minSampleShading min-sample-shading
	  %vk::pSampleMask p-sample-mask
	  %vk::alphaToCoverageEnable alpha-to-coverage-enable
	  %vk::alphaToOneEnable alpha-to-one-enable))
  (values))
  


(defun fill-graphics-pipeline-create-info (p-ci &key
						  (flags 0)
						  (stage-count 2)
						  p-stages
						  p-vertex-input-state
						  p-input-assembly-state
						  p-viewport-state
						  p-rasterization-state
						  p-multisample-state
						  p-depth-stencil-state
						  p-color-blend-state
						  p-dynamic-state
						  layout
						  render-pass
						  (subpass 0)
						  (base-pipeline-handle +nullptr+)
						  (base-pipeline-index -1)
						  &allow-other-keys)


  (with-foreign-slots ((%vk::flags
			%vk::stageCount
			%vk::pStages
			%vk::pVertexInputState
			%vk::pInputAssemblyState
			%vk::pViewportState
			%vk::pRasterizationState
			%vk::pMultisampleState
			%vk::pDepthStencilState
			%vk::pColorBlendState
			%vk::pDynamicState
			%vk::layout
			%vk::renderPass
			%vk::subpass
			%vk::basePipelineHandle
			%vk::basePipelineIndex)
		       p-ci
		       (:struct VkGraphicsPipelineCreateInfo))
    (setf %vk::flags flags
	  %vk::stageCount stage-count
	  %vk::pStages p-stages
	  %vk::pVertexInputState p-vertex-input-state
	  %vk::pInputAssemblyState p-input-assembly-state
	  %vk::pViewportState p-viewport-state
	  %vk::pRasterizationState p-rasterization-state
	  %vk::pMultisampleState p-multisample-state
	  %vk::pDepthStencilState p-depth-stencil-state
	  %vk::pColorBlendState p-color-blend-state
	  %vk::pDynamicState p-dynamic-state
	  %vk::layout layout
	  %vk::renderPass render-pass
	  %vk::subpass subpass
	  %vk::basePipelineHandle base-pipeline-handle
	  %vk::basePipelineIndex base-pipeline-index))
  (values))
  



(defun fill-pipeline-depth-stencil-state-create-info (p-ci
					     &key
					       (depth-test-enable VK_TRUE)
					       (depth-write-enable VK_TRUE)
					       (depth-compare-op VK_COMPARE_OP_LESS)
					       (depth-bounds-test-enable VK_FALSE)
					       (stencil-test-enable VK_FALSE)
					       &allow-other-keys)
  (with-foreign-slots ((%vk::depthTestEnable
			%vk::depthWriteEnable
			%vk::depthCompareOp
			%vk::depthBoundsTestEnable
			%vk::stencilTestEnable)
		       p-ci (:struct VkPipelineDepthStencilStateCreateInfo))
    (setf %vk::depthTestEnable depth-test-enable
	  %vk::depthWriteEnable depth-write-enable
	  %vk::depthCompareOp depth-compare-op
	  %vk::depthBoundsTestEnable depth-bounds-test-enable
	  %vk::stencilTestEnable stencil-test-enable))
  (values))



     

(defun fill-pipeline-layout-create-info (p-ci
					 &key
					   dsl
					   (push-constant-range-count 0)
					   (p-push-constant-ranges +nullptr+)
					   &allow-other-keys)
  (let ((p-set-layouts (foreign-alloc 'VkDescriptorSetLayout :count 1)))
    (setf (mem-aref p-set-layouts 'VkDescriptorSetLayout 0) (h dsl))
    (with-foreign-slots ((%vk::setLayoutCount
			  %vk::pSetLayouts
			  %vk::pushConstantRangeCount
			  %vk::pPushConstantRanges)
			 p-ci
			 (:struct VkPipelineLayoutCreateInfo))
      (setf %vk::setLayoutCount (if (null-pointer-p (h dsl)) 0 1) ;; hardcoded to one for now
	    %vk::pSetLayouts p-set-layouts
	    %vk::pushConstantRangeCount push-constant-range-count
	    %vk::pPushConstantRanges p-push-constant-ranges)))
  (values))

(defun fill-pipeline-color-blend-attachment-state (p-attachment
						   &key
						     (color-write-mask (logior VK_COLOR_COMPONENT_R_BIT VK_COLOR_COMPONENT_G_BIT
									       VK_COLOR_COMPONENT_B_BIT VK_COLOR_COMPONENT_A_BIT))
						     (blend-enable VK_FALSE)
						     (src-color-blend-factor VK_BLEND_FACTOR_SRC_ALPHA)
						     (dst-color-blend-factor VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA)
						     (color-blend-op VK_BLEND_OP_ADD)
						     (src-alpha-blend-factor VK_BLEND_FACTOR_ONE)
						     (dst-alpha-blend-factor VK_BLEND_FACTOR_ZERO)
						     (alpha-blend-op VK_BLEND_OP_ADD)
						     &allow-other-keys)
  
  (%vk::zero-struct p-attachment '(:struct VkPipelineColorBlendAttachmentState))
  
  (with-foreign-slots ((%vk::colorWriteMask
			%vk::blendEnable
			%vk::srcColorBlendFactor
			%vk::dstColorBlendFactor
			%vk::colorBlendOp
			%vk::srcAlphaBlendFactor
			%vk::dstAlphaBlendFactor
			%vk::alphaBlendOp)
		       p-attachment
		       (:struct VkPipelineColorBlendAttachmentState))
    (setf %vk::colorWriteMask color-write-mask
	  %vk::blendEnable blend-enable
	  %vk::srcColorBlendFactor src-color-blend-factor
	  %vk::dstColorBlendFactor dst-color-blend-factor
	  %vk::colorBlendOp color-blend-op
	  %vk::srcAlphaBlendFactor src-alpha-blend-factor
	  %vk::dstAlphaBlendFactor dst-alpha-blend-factor
	  %vk::alphaBlendOp alpha-blend-op))
  (values))

(defun fill-pipeline-dynamic-state-create-info (p-ci
						&key (dynamic-state-count 1)
						  p-dynamic-states
						  &allow-other-keys)
  (with-foreign-slots ((%vk::dynamicStateCount
			%vk::pDynamicStates)
		       p-ci
		       (:struct VkPipelineDynamicStateCreateInfo))
    (setf %vk::dynamicStateCount dynamic-state-count
	  %vk::pDynamicStates p-dynamic-states))
  (values))						       



(defun fill-pipeline-color-blend-state-create-info (p-ci
						    &key
						      (logic-op-enable VK_FALSE)
						      (logic-op VK_LOGIC_OP_COPY)
						      (attachment-count 1)
						      p-attachments
						      (blend-constant-r 0.0f0)
						      (blend-constant-g 0.0f0)
						      (blend-constant-b 0.0f0)
						      (blend-constant-a 0.0f0)
						      &allow-other-keys)
  (with-foreign-slots ((%vk::logicOpEnable
			%vk::logicOp
			%vk::attachmentCount
			%vk::pAttachments)
		       p-ci
		       (:struct VkPipelineColorBlendStateCreateInfo))
    (let ((p-blend-constants
	   (foreign-slot-pointer p-ci '(:struct VkPipelineColorBlendStateCreateInfo) '%vk::blendConstants)))
      (setf %vk::logicOpEnable logic-op-enable
	    %vk::logicOp logic-op
	    %vk::attachmentCount attachment-count ;;back-buffer-count
	    %vk::pAttachments p-attachments
	    (mem-aref p-blend-constants :float 0) blend-constant-r
	    (mem-aref p-blend-constants :float 1) blend-constant-g
	    (mem-aref p-blend-constants :float 2) blend-constant-b
	    (mem-aref p-blend-constants :float 3) blend-constant-a)))
  (values))

(defparameter *shader-entry-name* (cffi:foreign-string-alloc "main"))

(defun fill-pipeline-shader-stage-create-info (p-info &key
							stage
							module
							(p-name *shader-entry-name*)
							&allow-other-keys)
  (%vk::zero-struct p-info '(:struct VkPipelineShaderStageCreateInfo))
  (with-foreign-slots ((%vk::sType %vk::pNext %vk::flags %vk::stage %vk::module %vk::pName)
		       p-info
		       (:struct VkPipelineShaderStageCreateInfo))
    (setf %vk::sType VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
	  %vk::pNext +nullptr+
	  %vk::stage stage
	  %vk::module (h module)
	  %vk::pName p-name))
  (values))
