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



(defun create-logical-device (system-object
			      gpu &key (device-extensions (list VK_KHR_SWAPCHAIN_EXTENSION_NAME
								#+darwin "VK_KHR_portability_subset"))
				    (allocator +null-allocator+)
				    (graphics-queue-count 1)
				    (compute-queue-count 0)
				    (transfer-queue-count 0)
				    (sparse-binding-queue-count 0)
				    (rectangular-lines nil)
				    (bresenham-lines nil)
				    (smooth-lines nil)
				    (stippled-lines nil)
				    (enable-robust-buffer-access nil)
				    (enable-full-draw-index-uint32 nil)
				    (enable-image-cube-array nil)
				    (enable-independent-blend nil)
				    (enable-geometry-shader nil)
				    (enable-tessellation-shader nil)
				    (enable-sample-rate-shading nil)
				    (enable-dual-src-blend nil)
				    (enable-logic-op nil)
				    (enable-multi-draw-indirect nil)
				    (enable-draw-indirect-first-instance nil)
				    (enable-depth-clamp #-swiftshader t #+swiftshader nil)
				    (enable-depth-bias-clamp nil)
				    (enable-fill-mode-non-solid nil)
				    (enable-depth-bounds nil)
				    (enable-wide-lines t)
				    (enable-large-points t)
				    (enable-alpha-to-one nil)
				    (enable-multi-viewport nil)
				    (enable-sampler-anisotropy nil)
				    (enable-texture-compression-etc2 nil)
				    (enable-texture-compression-astc-ldr nil)
				    (enable-texture-compression-bc nil)
				    (enable-occlusion-query-precise nil)
				    (enable-pipeline-statistics-query nil)
				    (enable-vertex-pipeline-stores-and-atomics nil)
				    (enable-fragment-stores-and-atomics nil)
				    (enable-shader-tessellation-and-geometry-point-size nil)
				    (enable-shader-image-gather-extended nil)
				    (enable-shader-storage-image-extended-formats nil)
				    (enable-shader-storage-image-multisample nil)
				    (enable-shader-storage-image-read-without-format nil)
				    (enable-shader-storage-image-write-without-format nil)
				    (enable-shader-uniform-buffer-array-dynamic-indexing nil)
				    (enable-shader-sampled-image-array-dynamic-indexing nil)
				    (enable-shader-storage-buffer-array-dynamic-indexing nil)
				    (enable-shader-storage-image-array-dynamic-indexing nil)
				    (enable-shader-clip-distance nil)
				    (enable-shader-cull-distance nil)
				    (enable-shader-float64 nil)
				    (enable-shader-int64 t)
				    (enable-shader-int16 nil)
				    (enable-shader-resource-residency nil)
				    (enable-shader-resource-min-lod nil)
				    (enable-sparse-binding nil)
				    (enable-sparse-residency-buffer nil)
				    (enable-sparse-residency-image-2D nil)
				    (enable-sparse-residency-image-3D nil)
				    (enable-sparse-residency2-samples nil)
				    (enable-sparse-residency4-samples nil)
				    (enable-sparse-residency8-samples nil)
				    (enable-sparse-residency16-samples nil)
				    (enable-sparse-residency-aliased nil)
				    (enable-variable-multisample-rate nil)
				    (enable-inherited-queries nil)
				    (enable-sampler-mirror-clamp-to-edge nil)
				    (enable-draw-indirect-count nil)
				    (enable-storage-buffer-8-bit-access nil)
				    (enable-uniform-and-storage-buffer-8-bit-access nil)
				    (enable-storage-push-constant-8 nil)
				    (enable-shader-buffer-int64-atomics nil)
				    (enable-shader-shared-int64-atomics nil)
				    (enable-shader-float16 nil)
				    (enable-shader-int8 nil)
				    (enable-descriptor-indexing nil)
				    (enable-shader-input-attachment-array-dynamic-indexing nil)
				    (enable-shader-uniform-texel-buffer-array-dynamic-indexing nil)
				    (enable-shader-storage-texel-buffer-array-dynamic-indexing nil)
				    (enable-shader-uniform-buffer-array-non-uniform-indexing nil)
				    (enable-shader-sampled-image-array-non-uniform-indexing nil)
				    (enable-shader-storage-buffer-array-non-uniform-indexing nil)
				    (enable-shader-storage-image-array-non-uniform-indexing nil)
				    (enable-shader-input-attachment-array-non-uniform-indexing nil)
				    (enable-shader-uniform-texel-buffer-array-non-uniform-indexing nil)
				    (enable-shader-storage-texel-buffer-array-non-uniform-indexing nil)
				    (enable-descriptor-binding-uniform-buffer-update-after-bind nil)
				    (enable-descriptor-binding-sampled-image-update-after-bind nil)
				    (enable-descriptor-binding-storage-image-update-after-bind nil)
				    (enable-descriptor-binding-storage-buffer-update-after-bind nil)
				    (enable-descriptor-binding-uniform-texel-buffer-update-after-bind nil)
				    (enable-descriptor-binding-storage-texel-buffer-update-after-bind nil)
				    (enable-descriptor-binding-update-unused-while-pending nil)
				    (enable-descriptor-binding-partially-bound nil)
				    (enable-descriptor-binding-variable-descriptor-count nil)
				    (enable-runtime-descriptor-array nil)
				    (enable-sampler-filter-minmax nil)
				    (enable-scalar-block-layout nil)
				    (enable-imageless-framebuffer nil)
				    (enable-uniform-buffer-standard-layout nil)
				    (enable-shader-subgroup-extended-types nil)
				    (enable-separate-depth-stencil-layouts nil)
				    (enable-host-query-reset nil)
				    (enable-timeline-semaphore nil)
				    (enable-buffer-device-address t)
				    (enable-buffer-device-address-capture-replay nil)
				    (enable-buffer-device-address-multi-device nil)
				    (enable-vulkan-memory-model nil)
				    (enable-vulkan-memory-model-device-scope nil)
				    (enable-vulkan-memory-model-availability-visibility-chains nil)
				    (enable-shader-output-viewport-index nil)
				    (enable-shader-output-layer nil)
				    (enable-subgroup-broadcast-dynamic-id nil)
			      &allow-other-keys
			      &aux line-rasterization)
  (when (or rectangular-lines bresenham-lines smooth-lines stippled-lines)
    (warn "Physical device ~S may not have feature lineRasterization." gpu)
    (setq line-rasterization t))
  (when (and enable-robust-buffer-access (not (has-robust-buffer-access-p gpu)))
    (warn "Physical Device ~S does not support feature robustBufferAccess." gpu))
  (when (and enable-full-draw-index-uint32 (not (has-full-draw-index-uint32-p gpu)))
    (warn "Physical Device ~S does not support feature fullDrawIndexUint32." gpu))
  (when (and enable-image-cube-array (not (has-image-cube-array-p gpu)))
    (warn "Physical Device ~S does not support feature imageCubeArray." gpu))
  (when (and enable-independent-blend (not (has-independent-blend-p gpu)))
    (warn "Physical Device ~S does not support feature independentBlend." gpu))
  (when (and enable-geometry-shader (not (has-geometry-shader-p gpu)))
    (warn "Physical Device ~S does not support feature geometryShader." gpu))
  (when (and enable-tessellation-shader (not (has-tessellation-shader-p gpu)))
    (warn "Physical Device ~S does not support feature tessellationShader." gpu))
  (when (and enable-sample-rate-shading (not (has-sample-rate-shading-p gpu)))
    (warn "Physical Device ~S does not support feature sampleRateShading." gpu))
  (when (and enable-dual-src-blend (not (has-dual-src-blend-p gpu)))
    (warn "Physical Device ~S does not support feature dualSrcBlend." gpu))
  (when (and enable-logic-op (not (has-logic-op-p gpu)))
    (warn "Physical Device ~S does not support feature logicOp." gpu))
  (when (and enable-multi-draw-indirect (not (has-multi-draw-indirect-p gpu)))
    (warn "Physical Device ~S does not support feature multiDrawIndirect." gpu))
  (when (and enable-draw-indirect-first-instance (not (has-draw-indirect-first-instance-p gpu)))
    (warn "Physical Device ~S does not support feature drawIndirectFirstInstance." gpu))
  (when (and enable-depth-clamp (not (has-depth-clamp-p gpu)))
    (warn "Physical Device ~S does not support feature depthClamp." gpu))
  (when (and enable-depth-bias-clamp (not (has-depth-bias-clamp-p gpu)))
    (warn "Physical Device ~S does not support feature depthBiasClamp." gpu))
  (when (and enable-fill-mode-non-solid (not (has-fill-mode-non-solid-p gpu)))
    (warn "Physical Device ~S does not support feature fillModeNonSolid." gpu))
  (when (and enable-depth-bounds (not (has-depth-bounds-p gpu)))
    (warn "Physical Device ~S does not support feature depthBounds." gpu))
  (when (and enable-wide-lines (not (has-wide-lines-p gpu)))
    (warn "Physical Device ~S does not support feature wideLines." gpu))
  (when (and enable-large-points (not (has-large-points-p gpu)))
    (warn "Physical Device ~S does not support feature largePoints." gpu))
  (when (and enable-alpha-to-one (not (has-alpha-to-one-p gpu)))
    (warn "Physical Device ~S does not support feature alphaToOne." gpu))
  (when (and enable-multi-viewport (not (has-multi-viewport-p gpu)))
    (warn "Physical Device ~S does not support feature multiViewport." gpu))
  (when (and enable-sampler-anisotropy (not (has-sampler-anisotropy-p gpu)))
    (warn "Physical Device ~S does not support feature samplerAnisotropy." gpu))
  (when (and enable-texture-compression-etc2 (not (has-texture-compression-etc2-p gpu)))
    (warn "Physical Device ~S does not support feature textureCompressionETC2." gpu))
  (when (and enable-texture-compression-astc-ldr (not (has-texture-compression-astc-ldr-p gpu)))
    (warn "Physical Device ~S does not support feature textureCompressionASTC_LDR." gpu))
  (when (and enable-texture-compression-bc (not (has-texture-compression-bc-p gpu)))
    (warn "Physical Device ~S does not support feature textureCompressionBC." gpu))
  (when (and enable-occlusion-query-precise (not (has-occlusion-query-precise-p gpu)))
    (warn "Physical Device ~S does not support feature occlusionQueryPrecise." gpu))
  (when (and enable-pipeline-statistics-query (not (has-pipeline-statistics-query-p gpu)))
    (warn "Physical Device ~S does not support feature pipelineStatisticsQuery." gpu))
  (when (and enable-vertex-pipeline-stores-and-atomics (not (has-vertex-pipeline-stores-and-atomics-p gpu)))
    (warn "Physical Device ~S does not support feature vertexPipelineStoresAndAtomics." gpu))
  (when (and enable-fragment-stores-and-atomics (not (has-fragment-stores-and-atomics-p gpu)))
    (warn "Physical Device ~S does not support feature fragmentStoresAndAtomics." gpu))
  (when (and enable-shader-tessellation-and-geometry-point-size
	     (not (has-shader-tessellation-and-geometry-point-size-p gpu)))
    (warn "Physical Device ~S does not support feature shaderTessellationAndGeometryPointSize." gpu))
  (when (and enable-shader-image-gather-extended (not (has-shader-image-gather-extended-p gpu)))
    (warn "Physical Device ~S does not support feature shaderImageGatherExtended." gpu))
  (when (and enable-shader-storage-image-extended-formats (not (has-shader-storage-image-extended-formats-p gpu)))
    (warn "Physical Device ~S does not support feature shaderStorageImageExtendedFormats." gpu))
  (when (and enable-shader-storage-image-multisample (not (has-shader-storage-image-multisample-p gpu)))
    (warn "Physical Device ~S does not support feature shaderStorageImageMultisample." gpu))
  (when (and enable-shader-storage-image-read-without-format
	     (not (has-shader-storage-image-read-without-format-p gpu)))
    (warn "Physical Device ~S does not support feature shaderStorageImageReadWithoutFormat." gpu))
  (when (and enable-shader-storage-image-write-without-format
	     (not (has-shader-storage-image-write-without-format-p gpu)))
    (warn "Physical Device ~S does not support feature shaderStorageImageWriteWithoutFormat." gpu))
  (when (and enable-shader-uniform-buffer-array-dynamic-indexing
	     (not (has-shader-uniform-buffer-array-dynamic-indexing-p gpu)))
    (warn "Physical Device ~S does not support feature shaderUniformBufferArrayDynamicIndexing." gpu))
  (when (and enable-shader-sampled-image-array-dynamic-indexing
	     (not (has-shader-sampled-image-array-dynamic-indexing-p gpu)))
    (warn "Physical Device ~S does not support feature shaderSampledImageArrayDynamicIndexing." gpu))
  (when (and enable-shader-storage-buffer-array-dynamic-indexing
	     (not (has-shader-storage-buffer-array-dynamic-indexing-p gpu)))
    (warn "Physical Device ~S does not support feature shaderStorageBufferArrayDynamicIndexing." gpu))
  (when (and enable-shader-storage-image-array-dynamic-indexing
	     (not (has-shader-storage-image-array-dynamic-indexing-p gpu)))
    (warn "Physical Device ~S does not support feature shaderStorageImageArrayDynamicIndexing." gpu))
  (when (and enable-shader-clip-distance (not (has-shader-clip-distance-p gpu)))
    (warn "Physical Device ~S does not support feature shaderClipDistance." gpu))
  (when (and enable-shader-cull-distance (not (has-shader-cull-distance-p gpu)))
    (warn "Physical Device ~S does not support feature shaderCullDistance." gpu))
  (when (and enable-shader-float64 (not (has-shader-float64-p gpu)))
    (warn "Physical Device ~S does not support feature shaderFloat64." gpu))
  (when (and enable-shader-int64 (not (has-shader-int64-p gpu)))
    (warn "Physical Device ~S does not support feature shaderInt64." gpu))
  (when (and enable-shader-int16 (not (has-shader-int16-p gpu)))
    (warn "Physical Device ~S does not support feature shaderInt16." gpu))
  (when (and enable-shader-resource-residency (not (has-shader-resource-residency-p gpu)))
    (warn "Physical Device ~S does not support feature shaderResourceResidency." gpu))
  (when (and enable-shader-resource-min-lod (not (has-shader-resource-min-lod-p gpu)))
    (warn "Physical Device ~S does not support feature shaderResourceMinLod." gpu))
  (when (and enable-sparse-binding (not (has-sparse-binding-p gpu)))
    (warn "Physical Device ~S does not support feature sparseBinding." gpu))
  (when (and enable-sparse-residency-buffer (not (has-sparse-residency-buffer-p gpu)))
    (warn "Physical Device ~S does not support feature sparseResidencyBuffer." gpu))
  (when (and enable-sparse-residency-image-2D (not (has-sparse-residency-image-2D-p gpu)))
    (warn "Physical Device ~S does not support feature sparseResidencyImage2D." gpu))
  (when (and enable-sparse-residency-image-3D (not (has-sparse-residency-image-3D-p gpu)))
    (warn "Physical Device ~S does not support feature sparseResidencyImage3D." gpu))
  (when (and enable-sparse-residency2-samples (not (has-sparse-residency2-samples-p gpu)))
    (warn "Physical Device ~S does not support feature sparseResidency2Samples." gpu))
  (when (and enable-sparse-residency4-samples (not (has-sparse-residency4-samples-p gpu)))
    (warn "Physical Device ~S does not support feature sparseResidency4Samples." gpu))
  (when (and enable-sparse-residency8-samples (not (has-sparse-residency8-samples-p gpu)))
    (warn "Physical Device ~S does not support feature sparseResidency8Samples." gpu))
  (when (and enable-sparse-residency16-samples (not (has-sparse-residency16-samples-p gpu)))
    (warn "Physical Device ~S does not support feature sparseResidency16Samples." gpu))
  (when (and enable-sparse-residency-aliased (not (has-sparse-residency-aliased-p gpu)))
    (warn "Physical Device ~S does not support feature sparseResidencyAliased." gpu))
  (when (and enable-variable-multisample-rate (not (has-variable-multisample-rate-p gpu)))
    (warn "Physical Device ~S does not support feature variableMultisampleRate." gpu))
  (when (and enable-inherited-queries (not (has-inherited-queries-p gpu)))
    (warn "Physical Device ~S does not support feature inheritedQueries." gpu))
  (with-foreign-object (p-features2 '(:struct %vk::VkPhysicalDeviceFeatures2))
    (zero-struct p-features2 '(:struct %vk::VkPhysicalDeviceFeatures2))    
    (with-foreign-object (p-features12 '(:struct %vk::VkPhysicalDeviceVulkan12Features))
      (zero-struct p-features12 '(:struct %vk::VkPhysicalDeviceVulkan12Features))
      (with-foreign-slots ((%vk::sType
			    %vk::pNext)
			   p-features12
			   (:struct %vk::VkPhysicalDeviceVulkan12Features))
	(setf %vk::sType %vk::VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES
	      %vk::pNext (null-pointer)))
      (let ((p-features
	      (foreign-slot-pointer p-features2 '(:struct %vk::VkPhysicalDeviceFeatures2) '%vk::features)))
	(with-foreign-slots ((%vk::sType
			      %vk::pNext)
			     p-features2
			     (:struct %vk::VkPhysicalDeviceFeatures2))
	  (setf %vk::sType %vk::VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2
		%vk::pNext p-features12))
	(flet ((features-slot (slot-name slot-value)
		 (setf (foreign-slot-value p-features '(:struct VkPhysicalDeviceFeatures) slot-name)
		       (if (or (not slot-value) (and (integerp slot-value) (zerop slot-value))) VK_FALSE VK_TRUE)))
	       (features-slot12 (slot-name slot-value)
		 (setf (foreign-slot-value
			p-features12 '(:struct %vk::VkPhysicalDeviceVulkan12Features) slot-name)
		       (if (or (not slot-value)
			       (and (integerp slot-value)
				    (zerop slot-value)))
			   VK_FALSE VK_TRUE))))

	  (features-slot '%vk::robustBufferAccess enable-robust-buffer-access)
	  (features-slot '%vk::fullDrawIndexUint32 enable-full-draw-index-uint32)
	  (features-slot '%vk::imageCubeArray enable-image-cube-array)
	  (features-slot '%vk::independentBlend enable-independent-blend)
	  (features-slot '%vk::geometryShader enable-geometry-shader)
	  (features-slot '%vk::tessellationShader enable-tessellation-shader)
	  (features-slot '%vk::sampleRateShading enable-sample-rate-shading)
	  (features-slot '%vk::dualSrcBlend enable-dual-src-blend)
	  (features-slot '%vk::logicOp enable-logic-op)
	  (features-slot '%vk::multiDrawIndirect enable-multi-draw-indirect)
	  (features-slot '%vk::drawIndirectFirstInstance enable-draw-indirect-first-instance)
	  (features-slot '%vk::depthClamp enable-depth-clamp)
	  (features-slot '%vk::depthBiasClamp enable-depth-bias-clamp)
	  (features-slot '%vk::fillModeNonSolid enable-fill-mode-non-solid)
	  (features-slot '%vk::depthBounds enable-depth-bounds)
	  (features-slot '%vk::wideLines enable-wide-lines)
	  (features-slot '%vk::largePoints enable-large-points)
	  (features-slot '%vk::multiViewport enable-multi-viewport)
	  (features-slot '%vk::samplerAnisotropy enable-sampler-anisotropy)
	  (features-slot '%vk::textureCompressionETC2 enable-texture-compression-etc2)
	  (features-slot '%vk::textureCompressionASTC_LDR enable-texture-compression-astc-ldr)
	  (features-slot '%vk::textureCompressionBC enable-texture-compression-bc)
	  (features-slot '%vk::occlusionQueryPrecise enable-occlusion-query-precise)
	  (features-slot '%vk::pipelineStatisticsQuery enable-pipeline-statistics-query)
	  (features-slot '%vk::vertexPipelineStoresAndAtomics enable-vertex-pipeline-stores-and-atomics)
	  (features-slot '%vk::fragmentStoresAndAtomics enable-fragment-stores-and-atomics)
	  (features-slot '%vk::shaderTessellationAndGeometryPointSize enable-shader-tessellation-and-geometry-point-size)
	  (features-slot '%vk::shaderImageGatherExtended enable-shader-image-gather-extended)
	  (features-slot '%vk::shaderStorageImageExtendedFormats enable-shader-storage-image-extended-formats)
	  (features-slot '%vk::shaderStorageImageMultisample enable-shader-storage-image-multisample)
	  (features-slot '%vk::shaderStorageImageReadWithoutFormat enable-shader-storage-image-read-without-format)
	  (features-slot '%vk::shaderStorageImageWriteWithoutFormat enable-shader-storage-image-write-without-format)
	  (features-slot '%vk::shaderUniformBufferArrayDynamicIndexing enable-shader-uniform-buffer-array-dynamic-indexing)
	  (features-slot '%vk::shaderSampledImageArrayDynamicIndexing enable-shader-sampled-image-array-dynamic-indexing)
	  (features-slot '%vk::shaderStorageBufferArrayDynamicIndexing enable-shader-storage-buffer-array-dynamic-indexing)
	  (features-slot '%vk::shaderStorageImageArrayDynamicIndexing enable-shader-storage-image-array-dynamic-indexing)
	  (features-slot '%vk::shaderClipDistance enable-shader-clip-distance)
	  (features-slot '%vk::shaderCullDistance enable-shader-cull-distance)
	  (features-slot '%vk::shaderFloat64 enable-shader-float64)
	  (features-slot '%vk::shaderInt64 enable-shader-int64)
	  (features-slot '%vk::shaderInt16 enable-shader-int16)
	  (features-slot '%vk::shaderResourceResidency enable-shader-resource-residency)
	  (features-slot '%vk::shaderResourceMinLod enable-shader-resource-min-lod)
	  (features-slot '%vk::sparseBinding enable-sparse-binding)
	  (features-slot '%vk::sparseResidencyBuffer enable-sparse-residency-buffer)
	  (features-slot '%vk::sparseResidencyImage2D enable-sparse-residency-image-2D)
	  (features-slot '%vk::sparseResidencyImage3D enable-sparse-residency-image-3D)
	  (features-slot '%vk::sparseResidency2Samples enable-sparse-residency2-samples)
	  (features-slot '%vk::sparseResidency4Samples enable-sparse-residency4-samples)
	  (features-slot '%vk::sparseResidency8Samples enable-sparse-residency8-samples)
	  (features-slot '%vk::sparseResidency16Samples enable-sparse-residency16-samples)
	  (features-slot '%vk::sparseResidencyAliased enable-sparse-residency-aliased)
	  (features-slot '%vk::variableMultisampleRate enable-variable-multisample-rate)
	  (features-slot '%vk::inheritedQueries enable-inherited-queries)
	
	  (features-slot12 '%vk::samplerMirrorClampToEdge enable-sampler-mirror-clamp-to-edge)
	  (features-slot12 '%vk::drawIndirectCount enable-draw-indirect-count)
	  (features-slot12 '%vk::storageBuffer8BitAccess enable-storage-buffer-8-bit-access)
	  (features-slot12 '%vk::uniformAndStorageBuffer8BitAccess enable-uniform-and-storage-buffer-8-bit-access)	  
	  (features-slot12 '%vk::storagePushConstant8 enable-storage-push-constant-8)	  
	  (features-slot12 '%vk::shaderBufferInt64Atomics enable-shader-buffer-int64-atomics)	  
	  (features-slot12 '%vk::shaderSharedInt64Atomics enable-shader-shared-int64-atomics)	  
	  (features-slot12 '%vk::shaderFloat16 enable-shader-float16)	  
	  (features-slot12 '%vk::shaderInt8 enable-shader-int8)	  
	  (features-slot12 '%vk::descriptorIndexing enable-descriptor-indexing)	  
	  (features-slot12 '%vk::shaderInputAttachmentArrayDynamicIndexing enable-shader-input-attachment-array-dynamic-indexing)	  
	  (features-slot12 '%vk::shaderUniformTexelBufferArrayDynamicIndexing enable-shader-uniform-texel-buffer-array-dynamic-indexing)	  
	  (features-slot12 '%vk::shaderStorageTexelBufferArrayDynamicIndexing enable-shader-storage-texel-buffer-array-dynamic-indexing)	  
	  (features-slot12 '%vk::shaderUniformBufferArrayNonUniformIndexing enable-shader-uniform-buffer-array-non-uniform-indexing)	  
	  (features-slot12 '%vk::shaderSampledImageArrayNonUniformIndexing enable-shader-sampled-image-array-non-uniform-indexing)	  
	  (features-slot12 '%vk::shaderStorageBufferArrayNonUniformIndexing enable-shader-storage-buffer-array-non-uniform-indexing)	  
	  (features-slot12 '%vk::shaderStorageImageArrayNonUniformIndexing enable-shader-storage-image-array-non-uniform-indexing)	  
	  (features-slot12 '%vk::shaderInputAttachmentArrayNonUniformIndexing enable-shader-input-attachment-array-non-uniform-indexing)
	  (features-slot12 '%vk::shaderUniformTexelBufferArrayNonUniformIndexing enable-shader-uniform-texel-buffer-array-non-uniform-indexing)	  
	  (features-slot12 '%vk::shaderStorageTexelBufferArrayNonUniformIndexing enable-shader-storage-texel-buffer-array-non-uniform-indexing)	  
	  (features-slot12 '%vk::descriptorBindingUniformBufferUpdateAfterBind enable-descriptor-binding-uniform-buffer-update-after-bind)	  
	  (features-slot12 '%vk::descriptorBindingSampledImageUpdateAfterBind enable-descriptor-binding-sampled-image-update-after-bind)	  
	  (features-slot12 '%vk::descriptorBindingStorageImageUpdateAfterBind enable-descriptor-binding-storage-image-update-after-bind)	  
	  (features-slot12 '%vk::descriptorBindingStorageBufferUpdateAfterBind enable-descriptor-binding-storage-buffer-update-after-bind)	  
	  (features-slot12 '%vk::descriptorBindingUniformTexelBufferUpdateAfterBind enable-descriptor-binding-uniform-texel-buffer-update-after-bind)	  
	  (features-slot12 '%vk::descriptorBindingStorageTexelBufferUpdateAfterBind enable-descriptor-binding-storage-texel-buffer-update-after-bind)	  
	  (features-slot12 '%vk::descriptorBindingUpdateUnusedWhilePending enable-descriptor-binding-update-unused-while-pending)
	  (features-slot12 '%vk::descriptorBindingPartiallyBound enable-descriptor-binding-partially-bound)
	  (features-slot12 '%vk::descriptorBindingVariableDescriptorCount enable-descriptor-binding-variable-descriptor-count)	  
	  (features-slot12 '%vk::runtimeDescriptorArray enable-runtime-descriptor-array)	  
	  (features-slot12 '%vk::samplerFilterMinmax enable-sampler-filter-minmax)	  
	  (features-slot12 '%vk::scalarBlockLayout enable-scalar-block-layout)	  
	  (features-slot12 '%vk::imagelessFramebuffer enable-imageless-framebuffer)	  
	  (features-slot12 '%vk::uniformBufferStandardLayout enable-uniform-buffer-standard-layout)	  
	  (features-slot12 '%vk::shaderSubgroupExtendedTypes enable-shader-subgroup-extended-types)	  
	  (features-slot12 '%vk::separateDepthStencilLayouts enable-separate-depth-stencil-layouts)	  
	  (features-slot12 '%vk::hostQueryReset enable-host-query-reset)	  
	  (features-slot12 '%vk::timelineSemaphore enable-timeline-semaphore)	  
	  (features-slot12 '%vk::bufferDeviceAddress enable-buffer-device-address)	  
	  (features-slot12 '%vk::bufferDeviceAddressCaptureReplay enable-buffer-device-address-capture-replay)
	  
	  (features-slot12 '%vk::bufferDeviceAddressMultiDevice enable-buffer-device-address-multi-device)
	  (features-slot12 '%vk::vulkanMemoryModel enable-vulkan-memory-model)	  
	  (features-slot12 '%vk::vulkanMemoryModelDeviceScope enable-vulkan-memory-model-device-scope)	  
	  (features-slot12 '%vk::vulkanMemoryModelAvailabilityVisibilityChains enable-vulkan-memory-model-availability-visibility-chains)	  
	  (features-slot12 '%vk::shaderOutputViewportIndex enable-shader-output-viewport-index)	  
	  (features-slot12 '%vk::shaderOutputLayer enable-shader-output-layer)	  
	  (features-slot12 '%vk::subgroupBroadcastDynamicId enable-subgroup-broadcast-dynamic-id))
	(let ((device-extension-count (length device-extensions)))
	  (with-foreign-object (p-device-extensions :pointer device-extension-count)
	    (unwind-protect

		 (progn
		   (loop for i from 0 for extension in device-extensions
			 do (setf (mem-aref p-device-extensions :pointer i) (foreign-string-alloc extension)))
		   ;; todo: as it becomes clearer how multiple queues are used in Vulkan,
		   ;; rework and simplify this section of create-device
		   (labels ((dedicated? (desired-flag queue-family)
			      (eq desired-flag (queue-flags queue-family)))
			    (supports? (desired-flag queue-family)
			      (not (zerop (logand (queue-flags queue-family) desired-flag))))
			    (find-best (desired-flag desired-count)
			      (let ((dedicated-index (search (list desired-flag) (queue-families gpu)
							     :test #'(lambda (desired-flag queue-family)
								       (and (dedicated? desired-flag queue-family)
									    (>= (queue-count queue-family) desired-count)))))
				    (multipurpose-index (search (list desired-flag) (queue-families gpu)
								:test #'(lambda (desired-flag queue-family)
									  (and (supports? desired-flag queue-family)
									       (>= (queue-count queue-family) desired-count))))))
				(if dedicated-index
				    (list dedicated-index desired-count :dedicated)
				    (if multipurpose-index
					(list multipurpose-index desired-count :multipurpose))))))
				
		     (let* ((graphics-queue-family-index
			      (when (> graphics-queue-count 0) (find-best VK_QUEUE_GRAPHICS_BIT graphics-queue-count)))
			
			    (compute-queue-family-index
			      (when (> compute-queue-count 0) (find-best VK_QUEUE_COMPUTE_BIT compute-queue-count)))
			
			    (transfer-queue-family-index
			      (when (> transfer-queue-count 0) (find-best VK_QUEUE_TRANSFER_BIT transfer-queue-count)))
			
			    (sparse-binding-queue-family-index
			      (when (> sparse-binding-queue-count 0) (find-best VK_QUEUE_SPARSE_BINDING_BIT
										sparse-binding-queue-count)))
			    (queue-indices-and-totals
			      (loop for entry in (list graphics-queue-family-index
						       compute-queue-family-index
						       transfer-queue-family-index
						       sparse-binding-queue-family-index)
			            with result = (list :result)
				    ;; why do I feel like there is a simple map reduce way of solving this!
			            do (when entry
					 (let ((result-entry (assoc (first entry) (cdr result))))
					   (if (not result-entry)
				               (push (list (first entry) (second entry) (third entry)) (cdr result))
				               (setf (second result-entry) (+ (second entry) (second result-entry))))))
			            finally (return
				              (mapcar #'(lambda (entry)
							  (when entry
						            (let ((count (queue-count (elt (queue-families gpu) (car entry)))))
						              (when (> (cadr entry) count)
								(error "Not enough queues available at queue index ~a" (car entry))))
						            entry))
					              (cdr result))))))

		       (with-foreign-object (p-queue-infos '(:struct VkDeviceQueueCreateInfo)
					     (length queue-indices-and-totals))
			 (let ((allocs nil))
			   (unwind-protect
				(progn
				  (loop for queue in queue-indices-and-totals
					for x from 0
					do (zero-struct (mem-aptr p-queue-infos '(:struct VkDeviceQueueCreateInfo) x)
							'(:struct VkDeviceQueueCreateInfo))
					   (let ((p-queue-priorities (foreign-alloc :float :count (cadr queue))))
				             (push p-queue-priorities allocs)
				             (loop for i from 0 below (cadr queue)
						   do (setf (mem-aref p-queue-priorities :float i) 1.0f0))
				             (with-foreign-slots ((%vk::sType
								   %vk::queueFamilyIndex
								   %vk::queueCount
								   %vk::pQueuePriorities)
								  (mem-aptr p-queue-infos '(:struct VkDeviceQueueCreateInfo) x)
								  (:struct VkDeviceQueueCreateInfo))
				               (setf %vk::sType VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
					             %vk::queueFamilyIndex (car queue)
					             %vk::queueCount (cadr queue)
					             %vk::pQueuePriorities p-queue-priorities))))
			    
				  (with-vk-struct (p-create-info VkDeviceCreateInfo)
				    (let ((p-lr-features
					    (if line-rasterization
						(foreign-alloc
						 '(:struct %vk::VkPhysicalDeviceLineRasterizationFeaturesEXT))
						+nullptr+)))
				      (when line-rasterization
					(push p-lr-features allocs)
					(zero-struct
					 p-lr-features
					 '(:struct %vk::VkPhysicalDeviceLineRasterizationFeaturesEXT))
					(setf (foreign-slot-value
					       p-lr-features
					       '(:struct %vk::VkPhysicalDeviceLineRasterizationFeaturesEXT)
					       '%vk::sType)
					      %vk::VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT)
					(fill-physical-device-line-rasterization-features-ext
					 p-lr-features
					 :rectangular-lines (if rectangular-lines VK_TRUE VK_FALSE)
					 :bresenham-lines (if bresenham-lines VK_TRUE VK_FALSE)
					 :smooth-lines (if smooth-lines VK_TRUE VK_FALSE)
				       
					 :stippled-rectangular-lines
					 (if (and stippled-lines rectangular-lines) VK_TRUE VK_FALSE)
					 :stippled-bresenham-lines
					 (if (and stippled-lines bresenham-lines) VK_TRUE VK_FALSE)
					 :stippled-smooth-lines
					 (if (and stippled-lines smooth-lines) VK_TRUE VK_FALSE)))
				    
				      (with-foreign-slots ((%vk::pNext
							    %vk::queueCreateInfoCount
						            %vk::pQueueCreateInfos
						            %vk::enabledExtensionCount
						            %vk::ppEnabledExtensionNames
						            %vk::pEnabledFeatures)
							   p-create-info
							   (:struct VkDeviceCreateInfo))
					(setf %vk::pNext p-features2
					      %vk::pEnabledFeatures (null-pointer)
					      %vk::pQueueCreateInfos p-queue-infos
					      %vk::queueCreateInfoCount (length queue-indices-and-totals)
					      %vk::ppEnabledExtensionNames p-device-extensions
					      %vk::enabledExtensionCount device-extension-count))
				      (with-foreign-object (p-device 'VkDevice)
					(check-vk-result (vkCreateDevice (h gpu) p-create-info (h allocator) p-device))
					(let ((device (make-instance 'sgpu-device ;; todo put queue objects in device slots!
								     :handle (mem-aref p-device 'VkDevice)
								     :physical-device gpu
								     :allocator allocator)))
					  (push device (logical-devices (get-vulkan-instance system-object)))
					  (loop for queue in queue-indices-and-totals
						do
						   (push (list (first queue)
						               (loop for i from 0 below (second queue)
							             collect
							             (get-device-queue device (first queue) i (third queue))))
							 (device-queues device)))
					  device)))))

			     (loop for pointer in allocs do (foreign-free pointer))))))))
		 
	      (loop for i from 0 below device-extension-count
	            do (foreign-string-free (mem-aref p-device-extensions :pointer i))))))))))

(defun begin-single-time-commands (device command-pool)
  (with-vk-struct (p-alloc-info VkCommandBufferAllocateInfo)
    (with-foreign-slots ((%vk::level
			  %vk::commandPool
			  %vk::commandBufferCount)
			 p-alloc-info
			 (:struct VkCommandBufferAllocateInfo))
	(setf %vk::level VK_COMMAND_BUFFER_LEVEL_PRIMARY
	      %vk::commandPool (h command-pool)
	      %vk::commandBufferCount 1))
    (with-foreign-object (p-command-buffer 'VkCommandBuffer)
      (vkAllocateCommandBuffers (h device) p-alloc-info p-command-buffer)
      (let ((command-buffer (mem-aref p-command-buffer 'VkCommandBuffer)))
	(with-vk-struct (p-begin-info VkCommandBufferBeginInfo)
	  (setf (foreign-slot-value p-begin-info
				    '(:struct VkCommandBufferBeginInfo)
				    '%vk::flags)
		VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)

	  (vkBeginCommandBuffer command-buffer p-begin-info))
	  
	(make-instance 'command-buffer :handle command-buffer)))))

(defun end-single-time-commands (device command-pool queue command-buffer)
  (vkEndCommandBuffer command-buffer)

  (queue-submit1 queue command-buffer)

  (vkQueueWaitIdle (h queue))
  
  (with-foreign-object (p-command-buffer 'VkCommandBuffer)
    (setf (mem-aref p-command-buffer 'VkCommandBuffer) (h command-buffer))
    (vkFreeCommandBuffers (h device) (h command-pool) 1 p-command-buffer))

  (values))

(defun device-wait-idle (device)
  (check-vk-result (vkDeviceWaitIdle (h device))))

(defun compute-queue (device)
  (let ((gpu (physical-device device)))
    (flet ((fallback ()
	     (let ((multipurpose (get-any-queue-family-index-with-compute-support gpu)))
	       (if multipurpose
		   (let ((entry (assoc multipurpose (device-queues device))))
		     (when entry (values (first (second entry)) multipurpose)))))))
      
      (let ((dedicated (get-queue-family-index-with-dedicated-compute-support gpu)))
	(if dedicated
	    (let ((entry (assoc dedicated (device-queues device))))
	      (if entry
		  (values (first (second entry)) dedicated)
		  (fallback)))
	    (fallback))))))

(defun get-device-queue (device queue-family-index queue-index type)
  (with-foreign-object (p-queue 'VkQueue)
    (vkGetDeviceQueue (h device) queue-family-index queue-index p-queue)
    (make-instance (ecase type
		     (:dedicated 'dedicated-queue)
		     (:multipurpose 'multipurpose-queue))
		   :handle (mem-aref p-queue 'VkQueue)
		   :device device
		   :family-index queue-family-index
		   :index queue-index)))
