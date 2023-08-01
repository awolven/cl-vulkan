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

(defcstruct vec2
  (a :float)
  (b :float))

(defcstruct vec3
  (a :float)
  (b :float)
  (c :float))

(defcstruct Vertex
  (pos (:struct vec3))
  (color (:struct vec3)))

(defun create-graphics-pipeline (device pipeline-cache pipeline-layout render-pass back-buffer-count vertex-shader-module fragment-shader-module
                                 ;; todo: make create-pipeline configurable on vertex input attribute description options
				 &rest args
				 &key (allocator +null-allocator+)
				   (geometry-shader-module nil)
				   (tessellation-control-shader-module nil)
				   (tessellation-evaluation-shader-module nil)
				   (vertex-type '(:struct Vertex))
				   (vertex-size (if vertex-type (foreign-type-size vertex-type) 0))
				   (vertex-input-attribute-descriptions
				    (list (make-instance 'vertex-input-attribute-description
							 :location 0
							 :offset (foreign-slot-offset vertex-type 'pos))
					  (make-instance 'vertex-input-attribute-description
							 :location 1
							 :offset (foreign-slot-offset vertex-type 'color))))
				 &allow-other-keys)
  (declare (ignore back-buffer-count))
  (let ((shader-module-count (length (remove-if #'null (list vertex-shader-module fragment-shader-module geometry-shader-module
							     tessellation-control-shader-module tessellation-evaluation-shader-module)))))
    (with-foreign-object (p-shader-stages '(:struct VkPipelineShaderStageCreateInfo) shader-module-count)
      (let ((i -1))

	(when vertex-shader-module
	  (fill-pipeline-shader-stage-create-info (mem-aptr p-shader-stages '(:struct VkPipelineShaderStageCreateInfo) (incf i))
						  :stage VK_SHADER_STAGE_VERTEX_BIT
						  :module vertex-shader-module))
	(when fragment-shader-module
	  (fill-pipeline-shader-stage-create-info (mem-aptr p-shader-stages '(:struct VkPipelineShaderStageCreateInfo) (incf i))
						  :stage VK_SHADER_STAGE_FRAGMENT_BIT
						  :module fragment-shader-module))
	(when geometry-shader-module
	  (fill-pipeline-shader-stage-create-info (mem-aptr p-shader-stages '(:struct VkPipelineShaderStageCreateInfo) (incf i))
						  :stage VK_SHADER_STAGE_GEOMETRY_BIT
						  :module geometry-shader-module))
	(when tessellation-control-shader-module
	  (fill-pipeline-shader-stage-create-info (mem-aptr p-shader-stages '(:struct VkPipelineShaderStageCreateInfo) (incf i))
						  :stage VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT
						  :module tessellation-control-shader-module))
	(when tessellation-evaluation-shader-module
	  (fill-pipeline-shader-stage-create-info (mem-aptr p-shader-stages '(:struct VkPipelineShaderStageCreateInfo) (incf i))
						  :stage VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT
						  :module tessellation-evaluation-shader-module)))
      (with-vertex-input-binding-description (p-vibd)
	(apply #'fill-vertex-input-binding-description p-vibd (append args (list :stride vertex-size)))
	(with-foreign-object (p-attribute-descriptions '(:struct VkVertexInputAttributeDescription)
			      (length vertex-input-attribute-descriptions))
	  (loop for description in vertex-input-attribute-descriptions
	        for i from 0
	        do (fill-vertex-input-attribute-description
		    (mem-aptr p-attribute-descriptions '(:struct VkVertexInputAttributeDescription) i)
		    :location (location description)
		    :binding (binding description)
		    :format (desc-format description)
		    :offset (offset description)))
	  (with-pipeline-vertex-input-state-create-info (p-pvisci)
	    (apply #'fill-pipeline-vertex-input-state-create-info p-pvisci
		   :vertex-binding-description-count (if vertex-type 1 0)
		   :p-vertex-binding-descriptions (if vertex-type p-vibd +nullptr+)
		   :vertex-attribute-description-count (length vertex-input-attribute-descriptions)
		   :p-vertex-attribute-descriptions (if vertex-type p-attribute-descriptions +nullptr+)
		   args)
	    
	    (with-pipeline-input-assembly-state-create-info (p-piasci)
	      (apply #'fill-pipeline-input-assembly-state-create-info p-piasci args)
	      
	      (with-viewport-structure (p-viewport)
		(apply #'fill-viewport-structure p-viewport args)
		
		(with-scissor-structure (p-scissor)
		  (apply #'fill-scissor-structure p-scissor args)
		  
		  (with-pipeline-viewport-state-create-info (p-viewport-state)
		    (apply #'fill-pipeline-viewport-state-create-info p-viewport-state
			   :viewport-count 1
			   :p-viewports +nullptr+ ;; p-viewport
			   :scissor-count 1
			   :p-scissors +nullptr+ ;;p-scissor
			   args)

		    (with-pipeline-rasterization-line-state-create-info-ext (p-lscie)
		      (apply #'fill-pipeline-rasterization-line-state-create-info-ext
			     p-lscie :lineRasterizationMode %vk::VK_LINE_RASTERIZATION_MODE_RECTANGULAR_EXT args)
		    
		      (with-pipeline-rasterization-state-create-info (p-rasterizer)
			(apply #'fill-pipeline-rasterization-state-create-info p-rasterizer
			       args)
			(when (and (getf args :stippled-line-enable)
				   (not (zerop (getf args :stippled-line-enable))))
			  (setf (foreign-slot-value p-rasterizer
						    '(:struct VkPipelineRasterizationStateCreateInfo)
						    '%vk::pNext) p-lscie))
		      
			(with-pipeline-multisample-state-create-info (p-multisampling)
			  (apply #'fill-pipeline-multisample-state-create-info p-multisampling args)
			
			  (with-foreign-object (p-color-blend-attachments
						'(:struct VkPipelineColorBlendAttachmentState))
			    (apply #'fill-pipeline-color-blend-attachment-state
				   p-color-blend-attachments args)
		      
			    (with-pipeline-color-blend-state-create-info  (p-color-blending)
			      (apply #'fill-pipeline-color-blend-state-create-info p-color-blending
				     :p-attachments p-color-blend-attachments
				     :attachment-count 1
				     args)
			      (let ((dynamic-state-count
				      #-(or darwin) 4
				      #+(or darwin) 2))
				(with-dynamic-states (p-dynamic-states dynamic-state-count)

				  (setf (mem-aref p-dynamic-states 'VkDynamicState 0) VK_DYNAMIC_STATE_VIEWPORT
					(mem-aref p-dynamic-states 'VkDynamicState 1) VK_DYNAMIC_STATE_SCISSOR)
				  #-(or darwin)
				  (setf (mem-aref p-dynamic-states 'VKDynamicState 2) %vk::VK_DYNAMIC_STATE_LINE_STIPPLE_EXT
					(mem-aref p-dynamic-states 'VKDynamicState 3) VK_DYNAMIC_STATE_LINE_WIDTH)
		      
				  (with-pipeline-dynamic-state-create-info (p-pipeline-dynamic-state-ci)
				    (apply #'fill-pipeline-dynamic-state-create-info p-pipeline-dynamic-state-ci
					   :dynamic-state-count dynamic-state-count
					   :p-dynamic-states p-dynamic-states args)
			
				    (with-pipeline-depth-stencil-state-create-info (p-depth-stencil)
				      (apply #'fill-pipeline-depth-stencil-state-create-info p-depth-stencil args)
				      (with-graphics-pipeline-create-info (p-pipeline-ci)
					(apply #'fill-graphics-pipeline-create-info p-pipeline-ci
					       :stage-count shader-module-count
					       :p-stages p-shader-stages
					       :p-vertex-input-state p-pvisci
					       :p-input-assembly-state p-piasci
					       :p-viewport-state p-viewport-state
					       :p-rasterization-state p-rasterizer
					       :p-multisample-state p-multisampling
					       :p-depth-stencil-state p-depth-stencil
					       :p-color-blend-state p-color-blending
					       :p-dynamic-state p-pipeline-dynamic-state-ci
					       :layout (h pipeline-layout)
					       :render-pass (h render-pass)
					       args)

					(with-foreign-object (p-graphics-pipeline 'VkPipeline)
					  (check-vk-result
					   (vkCreateGraphicsPipelines
					    (h device) (h pipeline-cache) 1 p-pipeline-ci (h allocator) p-graphics-pipeline))
					  (make-instance 'graphics-pipeline :handle (mem-aref p-graphics-pipeline 'VkPipeline)
							                    :device device
							                    :allocator allocator
							                    :vertex-shader vertex-shader-module
							                    :geometry-shader geometry-shader-module
							                    :fragment-shader fragment-shader-module))))))))))))))))))))))

(defun destroy-pipeline (pipeline)
  (with-slots (device allocator) pipeline
    (vkDestroyPipeline (h device) (h pipeline) (h allocator)))
  (values))
