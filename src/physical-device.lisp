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

(defcstruct %vk::VkPhysicalDeviceFeatures2
  (%vk::sType %vk::VkStructureType)
  (%vk::pNext :pointer)
  (%vk::features (:struct %vk::VkPhysicalDeviceFeatures)))

(defconstant %vk::VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 1000059000)
(defconstant %vk::VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES 51)

(defcfun (%vk::vkGetPhysicalDeviceFeatures2 "vkGetPhysicalDeviceFeatures2")
    :void
  (physicalDevice %vk::VkPhysicalDevice)
  (pFeatures :pointer))

(defcstruct %vk::VkPhysicalDeviceVulkan12Features
  (%vk::sType                                                        %vk::VkStructureType)
  (%vk::pNext                                                        :pointer)
  (%vk::samplerMirrorClampToEdge                                     %vk::VkBool32)
  (%vk::drawIndirectCount                                            %vk::VkBool32)
  (%vk::storageBuffer8BitAccess                                      %vk::VkBool32)
  (%vk::uniformAndStorageBuffer8BitAccess                            %vk::VkBool32)
  (%vk::storagePushConstant8                                         %vk::VkBool32)
  (%vk::shaderBufferInt64Atomics                                     %vk::VkBool32)
  (%vk::shaderSharedInt64Atomics                                     %vk::VkBool32)
  (%vk::shaderFloat16                                                %vk::VkBool32)
  (%vk::shaderInt8                                                   %vk::VkBool32)
  (%vk::descriptorIndexing                                           %vk::VkBool32)
  (%vk::shaderInputAttachmentArrayDynamicIndexing                    %vk::VkBool32)
  (%vk::shaderUniformTexelBufferArrayDynamicIndexing                 %vk::VkBool32)
  (%vk::shaderStorageTexelBufferArrayDynamicIndexing                 %vk::VkBool32)
  (%vk::shaderUniformBufferArrayNonUniformIndexing                   %vk::VkBool32)
  (%vk::shaderSampledImageArrayNonUniformIndexing                    %vk::VkBool32)
  (%vk::shaderStorageBufferArrayNonUniformIndexing                   %vk::VkBool32)
  (%vk::shaderStorageImageArrayNonUniformIndexing                    %vk::VkBool32)
  (%vk::shaderInputAttachmentArrayNonUniformIndexing                 %vk::VkBool32)
  (%vk::shaderUniformTexelBufferArrayNonUniformIndexing              %vk::VkBool32)
  (%vk::shaderStorageTexelBufferArrayNonUniformIndexing              %vk::VkBool32)
  (%vk::descriptorBindingUniformBufferUpdateAfterBind                %vk::VkBool32)
  (%vk::descriptorBindingSampledImageUpdateAfterBind                 %vk::VkBool32)
  (%vk::descriptorBindingStorageImageUpdateAfterBind                 %vk::VkBool32)
  (%vk::descriptorBindingStorageBufferUpdateAfterBind                %vk::VkBool32)
  (%vk::descriptorBindingUniformTexelBufferUpdateAfterBind           %vk::VkBool32)
  (%vk::descriptorBindingStorageTexelBufferUpdateAfterBind           %vk::VkBool32)
  (%vk::descriptorBindingUpdateUnusedWhilePending                    %vk::VkBool32)
  (%vk::descriptorBindingPartiallyBound                              %vk::VkBool32)
  (%vk::descriptorBindingVariableDescriptorCount                     %vk::VkBool32)
  (%vk::runtimeDescriptorArray                                       %vk::VkBool32)
  (%vk::samplerFilterMinmax                                          %vk::VkBool32)
  (%vk::scalarBlockLayout                                            %vk::VkBool32)
  (%vk::imagelessFramebuffer                                         %vk::VkBool32)
  (%vk::uniformBufferStandardLayout                                  %vk::VkBool32)
  (%vk::shaderSubgroupExtendedTypes                                  %vk::VkBool32)
  (%vk::separateDepthStencilLayouts                                  %vk::VkBool32)
  (%vk::hostQueryReset                                               %vk::VkBool32)
  (%vk::timelineSemaphore                                            %vk::VkBool32)
  (%vk::bufferDeviceAddress                                          %vk::VkBool32)
  (%vk::bufferDeviceAddressCaptureReplay                             %vk::VkBool32)
  (%vk::bufferDeviceAddressMultiDevice                               %vk::VkBool32)
  (%vk::vulkanMemoryModel                                            %vk::VkBool32)
  (%vk::vulkanMemoryModelDeviceScope                                 %vk::VkBool32)
  (%vk::vulkanMemoryModelAvailabilityVisibilityChains                %vk::VkBool32)
  (%vk::shaderOutputViewportIndex                                    %vk::VkBool32)
  (%vk::shaderOutputLayer                                            %vk::VkBool32)
  (%vk::subgroupBroadcastDynamicId                                   %vk::VkBool32))

(defun get-queue-family-index-with-dedicated-compute-support (gpu)
  (loop for i from 0 for queue-family in (queue-families gpu)
     do (when (eq VK_QUEUE_COMPUTE_BIT (queue-flags queue-family))
	  (return-from get-queue-family-index-with-dedicated-compute-support i))
     finally (return nil)))

(defun get-any-queue-family-index-with-compute-support (gpu)
  (loop for i from 0 for queue-family in (queue-families gpu)
     do (when (compute-queue-family-p queue-family)
	  (return-from get-any-queue-family-index-with-compute-support i))
     finally (return nil)))

(defun get-queue-family-index-with-dedicated-transfer-support (gpu)
  (loop for i from 0 for queue-family in (queue-families gpu)
     do (when (eq VK_QUEUE_TRANSFER_BIT (queue-flags queue-family))
	  (return-from get-queue-family-index-with-dedicated-transfer-support i))
     finally (return nil)))

(defun get-any-queue-family-index-with-transfer-support (gpu)
  (loop for i from 0 for queue-family in (queue-families gpu)
     do (when (transfer-queue-family-p queue-family)
	  (return-from get-any-queue-family-index-with-transfer-support i))
     finally (return nil)))

(defun get-physical-device-queue-family-properties (gpu)
  (with-foreign-object (p-queue-family-property-count :uint32)
    (vkGetPhysicalDeviceQueueFamilyProperties (h gpu) p-queue-family-property-count +nullptr+)
    (let ((count (mem-aref p-queue-family-property-count :uint32)))
      (with-foreign-object (p-queue-family-properties '(:struct VkQueueFamilyProperties) count)
	(vkGetPhysicalDeviceQueueFamilyProperties (h gpu) p-queue-family-property-count p-queue-family-properties)
	(loop for i from 0 below (mem-aref p-queue-family-property-count :uint32)
	   collect (with-foreign-slots ((%vk::queueFlags
					 %vk::queueCount
					 %vk::timestampValidBits)
					(mem-aptr p-queue-family-properties '(:struct VkQueueFamilyProperties) i)
					(:struct VkQueueFamilyProperties))
		     (make-instance 'queue-family
				    :queue-flags %vk::queueFlags ;; capabilities of these queues
				    :queue-count %vk::queueCount ;; number of queues in queue family
				    :timestamp-valid-bits %vk::timestampValidBits
				    :min-image-transfer-granularity
				    (make-instance 'extent-3d
						   :width (foreign-slot-value (foreign-slot-pointer p-queue-family-properties
												    '(:struct VkQueueFamilyProperties)
												    '%vk::minImageTransferGranularity)
									      '(:struct VkExtent3D) '%vk::width)
						   :height (foreign-slot-value (foreign-slot-pointer p-queue-family-properties
												    '(:struct VkQueueFamilyProperties)
												    '%vk::minImageTransferGranularity)
									       '(:struct VkExtent3D) '%vk::height)
						   :depth (foreign-slot-value (foreign-slot-pointer p-queue-family-properties
												    '(:struct VkQueueFamilyProperties)
												    '%vk::minImageTransferGranularity)
									      '(:struct VkExtent3D) '%vk::depth)))))))))

(defun enumerate-physical-devices (system-object)
  (let ((instance (get-vulkan-instance system-object)))
    (with-foreign-object (p-count :uint32)
      (check-vk-result (vkEnumeratePhysicalDevices (h instance) p-count +nullptr+))
      (let ((count (mem-aref p-count :uint32)))
	(with-foreign-object (p-gpus 'VkPhysicalDevice count)
	  (check-vk-result (vkEnumeratePhysicalDevices (h instance) p-count p-gpus))
	  (loop for i from 0 below (mem-aref p-count :uint32)
	     collect (with-foreign-object (p-properties '(:struct VkPhysicalDeviceProperties))
		       (vkGetPhysicalDeviceProperties (mem-aref p-gpus 'VkPhysicalDevice i) p-properties)
		       (with-foreign-slots ((%vk::apiVersion
					     %vk::driverVersion
					     %vk::vendorID
					     %vk::deviceID
					     %vk::deviceType
					     %vk::deviceName
					     %vk::pipelineCacheUUID
					     %vk::sparseProperties)
					    p-properties (:struct VkPhysicalDeviceProperties))
			 (let ((p-limits (foreign-slot-pointer p-properties '(:struct VkPhysicalDeviceProperties) '%vk::limits)))
			   (with-foreign-object (p-features2 '(:struct %vk::VkPhysicalDeviceFeatures2))
			     (with-foreign-object (p-features12 '(:struct %vk::VkPhysicalDeviceVulkan12Features))
			       (with-foreign-slots ((%vk::sType
						     %vk::pNext)
						    p-features2
						    (:struct %vk::VkPhysicalDeviceFeatures2))
				 (setf %vk::sType %vk::VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2
				       %vk::pNext p-features12))
			       (with-foreign-slots ((%vk::sType
						     %vk::pNext)
						    p-features12
						    (:struct %vk::VkPhysicalDeviceVulkan12Features))
				 (setf %vk::sType %vk::VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES
				       %vk::pNext (null-pointer)))
				 
				 
			       (let ((p-features (foreign-slot-pointer p-features2 '(:struct %vk::VkPhysicalDeviceFeatures2) '%vk::features)))
			       
			       (%vk::vkGetPhysicalDeviceFeatures2 (mem-aref p-gpus 'VkPhysicalDevice i) p-features2)
			       (flet ((feature-slot (slot-name)
					(unless (zerop (foreign-slot-value p-features '(:struct VkPhysicalDeviceFeatures) slot-name))
					  t))
				      (feature-slot12 (slot-name)
					(unless (zerop (foreign-slot-value p-features12 '(:struct %vk::VkPhysicalDeviceVulkan12Features) slot-name))
					  t)))
					
				 (let ((gpu
					 (make-instance 'physical-device
							:handle (mem-aref p-gpus 'VkPhysicalDevice i)
							:index i
							:api-version %vk::apiVersion
							:driver-version %vk::driverVersion
							:vendor-id %vk::vendorID
							:device-id %vk::deviceID
							:device-type %vk::deviceType
							:device-name (foreign-string-to-lisp %vk::deviceName :encoding :utf-8)
							:memory-heaps (%get-memory-heaps (mem-aref p-gpus 'VkPhysicalDevice i))
							:memory-types (%get-memory-types (mem-aref p-gpus 'VkPhysicalDevice i))
							:pipeline-cache-uuid
							(make-array VK_UUID_SIZE :element-type '(unsigned-byte 8)
										 :initial-contents
										 (loop for i from 0 below VK_UUID_SIZE
										       collect (mem-aref (foreign-slot-pointer
													  p-properties
													  '(:struct VkPhysicalDeviceProperties)
													  '%vk::pipelineCacheUUID)
													 :unsigned-char i)))
							:robust-buffer-access
							(feature-slot '%vk::robustBufferAccess)
							:full-draw-index-uint32
							(feature-slot '%vk::fullDrawIndexUint32)
							:image-cube-array
							(feature-slot '%vk::imageCubeArray)
							:independent-blend
							(feature-slot '%vk::independentBlend)
							:geometry-shader
							(feature-slot '%vk::geometryShader)
							:tessellation-shader
							(feature-slot '%vk::tessellationShader)
							:sample-rate-shading
							(feature-slot '%vk::sampleRateShading)
							:dual-src-blend
							(feature-slot '%vk::dualSrcBlend)
							:logic-op
							(feature-slot '%vk::logicOp)
							:multi-draw-indirect
							(feature-slot '%vk::multiDrawIndirect)
							:draw-indirect-first-instance
							(feature-slot '%vk::drawIndirectFirstInstance)
							:depth-clamp
							(feature-slot '%vk::depthClamp)
							:depth-bias-clamp
							(feature-slot '%vk::depthBiasClamp)
							:fill-mode-non-solid
							(feature-slot '%vk::fillModeNonSolid)
							:depth-bounds
							(feature-slot '%vk::depthBounds)
							:wide-lines
							(feature-slot '%vk::wideLines)
							:large-points
							(feature-slot '%vk::largePoints)
							:alpha-to-one
							(feature-slot '%vk::alphaToOne)
							:multi-viewport
							(feature-slot '%vk::multiViewport)
							:sampler-anisotropy
							(feature-slot '%vk::samplerAnisotropy)
							:texture-compression-etc2
							(feature-slot '%vk::textureCompressionETC2)
							:texture-compression-astc-ldr
							(feature-slot '%vk::textureCompressionASTC_LDR)
							:texture-compression-bc
							(feature-slot '%vk::textureCompressionBC)
							:occlusion-query-precise
							(feature-slot '%vk::occlusionQueryPrecise)
							:pipeline-statistics-query
							(feature-slot '%vk::pipelineStatisticsQuery)
							:vertex-pipeline-stores-and-atomics
							(feature-slot '%vk::vertexPipelineStoresAndAtomics)
							:fragment-stores-and-atomics
							(feature-slot '%vk::fragmentStoresAndAtomics)
							:shader-tessellation-and-geometry-point-size
							(feature-slot '%vk::shaderTessellationAndGeometryPointSize)
							:shader-image-gather-extended
							(feature-slot '%vk::shaderImageGatherExtended)
							:shader-storage-image-extended-formats
							(feature-slot '%vk::shaderStorageImageExtendedFormats)
							:shader-storage-image-multisample
							(feature-slot '%vk::shaderStorageImageMultisample)
							:shader-storage-image-read-without-format
							(feature-slot '%vk::shaderStorageImageReadWithoutFormat)
							:shader-storage-image-write-without-format
							(feature-slot '%vk::shaderStorageImageWriteWithoutFormat)
							:shader-uniform-buffer-array-dynamic-indexing
							(feature-slot '%vk::shaderUniformBufferArrayDynamicIndexing)
							:shader-sampled-image-array-dynamic-indexing
							(feature-slot '%vk::shaderSampledImageArrayDynamicIndexing)
							:shader-storage-buffer-array-dynamic-indexing
							(feature-slot '%vk::shaderStorageBufferArrayDynamicIndexing)
							:shader-storage-image-array-dynamic-indexing
							(feature-slot '%vk::shaderStorageImageArrayDynamicIndexing)
							:shader-clip-distance
							(feature-slot '%vk::shaderClipDistance)
							:shader-cull-distance
							(feature-slot '%vk::shaderCullDistance)
							:shader-float64
							(feature-slot '%vk::shaderFloat64)
							:shader-int64
							(feature-slot '%vk::shaderInt64)
							:shader-int16
							(feature-slot '%vk::shaderInt16)
							:shader-resource-residency
							(feature-slot '%vk::shaderResourceResidency)
							:shader-resource-min-lod
							(feature-slot '%vk::shaderResourceMinLod)
							:sparse-binding
							(feature-slot '%vk::sparseBinding)
							:sparse-residency-buffer
							(feature-slot '%vk::sparseResidencyBuffer)
							:sparse-residency-image-2D
							(feature-slot '%vk::sparseResidencyImage2D)
							:sparse-residency-image-3D
							(feature-slot '%vk::sparseResidencyImage3D)
							:sparse-residency2-samples
							(feature-slot '%vk::sparseResidency2Samples)
							:sparse-residency4-samples
							(feature-slot '%vk::sparseResidency4Samples)
							:sparse-residency8-samples
							(feature-slot '%vk::sparseResidency8Samples)
							:sparse-residency16-samples
							(feature-slot '%vk::sparseResidency16Samples)
							:sparse-residency-aliased
							(feature-slot '%vk::sparseResidencyAliased)
							:variable-multisample-rate
							(feature-slot '%vk::variableMultisampleRate)
							:inherited-queries
							(feature-slot '%vk::inheritedQueries)
							
							:sampler-mirror-clamp-to-edge
							(feature-slot12 '%vk::samplerMirrorClampToEdge)
							:draw-indirect-count
							(feature-slot12 '%vk::drawIndirectCount)
							:storage-buffer-8-bit-access
							(feature-slot12 '%vk::storageBuffer8BitAccess)
							:uniform-and-storage-buffer-8-bit-access
							(feature-slot12 '%vk::uniformAndStorageBuffer8BitAccess)
							:storage-push-constant-8
							(feature-slot12 '%vk::storagePushConstant8)
							:shader-buffer-int64-atomics
							(feature-slot12 '%vk::shaderBufferInt64Atomics)
							:shader-shared-int64-atomics
							(feature-slot12 '%vk::shaderSharedInt64Atomics)
							:shader-float16
							(feature-slot12 '%vk::shaderFloat16)
							:shader-int8
							(feature-slot12 '%vk::shaderInt8)
							:descriptor-indexing
							(feature-slot12 '%vk::descriptorIndexing)
							:shader-input-attachment-array-dynamic-indexing
							(feature-slot12 '%vk::shaderInputAttachmentArrayDynamicIndexing)
							:shader-uniform-texel-buffer-array-dynamic-indexing
							(feature-slot12 '%vk::shaderUniformTexelBufferArrayDynamicIndexing)
							:shader-storage-texel-buffer-array-dynamic-indexing
							(feature-slot12 '%vk::shaderStorageTexelBufferArrayDynamicIndexing)
							:shader-uniform-buffer-array-non-uniform-indexing
							(feature-slot12 '%vk::shaderUniformBufferArrayNonUniformIndexing)
							:shader-sampled-image-array-non-uniform-indexing
							(feature-slot12 '%vk::shaderSampledImageArrayNonUniformIndexing)
							:shader-storage-buffer-array-non-uniform-indexing
							(feature-slot12 '%vk::shaderStorageBufferArrayNonUniformIndexing)
							:shader-storage-image-array-non-uniform-indexing
							(feature-slot12 '%vk::shaderStorageImageArrayNonUniformIndexing)
							:shader-input-attachment-array-non-uniform-indexing
							(feature-slot12 '%vk::shaderInputAttachmentArrayNonUniformIndexing)
							:shader-uniform-texel-buffer-array-non-uniform-indexing
							(feature-slot12 '%vk::shaderUniformTexelBufferArrayNonUniformIndexing)
							:shader-storage-texel-buffer-array-non-uniform-indexing
							(feature-slot12 '%vk::shaderStorageTexelBufferArrayNonUniformIndexing)
							:descriptor-binding-uniform-buffer-update-after-bind
							(feature-slot12 '%vk::descriptorBindingUniformBufferUpdateAfterBind)
							:descriptor-binding-sampled-image-update-after-bind
							(feature-slot12 '%vk::descriptorBindingSampledImageUpdateAfterBind)
							:descriptor-binding-storage-image-update-after-bind
							(feature-slot12 '%vk::descriptorBindingStorageImageUpdateAfterBind)
							:descriptor-binding-storage-buffer-update-after-bind
							(feature-slot12 '%vk::descriptorBindingStorageBufferUpdateAfterBind)
							:descriptor-binding-uniform-texel-buffer-update-after-bind
							(feature-slot12 '%vk::descriptorBindingUniformTexelBufferUpdateAfterBind)
							:descriptor-binding-storage-texel-buffer-update-after-bind
							(feature-slot12 '%vk::descriptorBindingStorageTexelBufferUpdateAfterBind)
							:descriptor-binding-update-unused-while-pending
							(feature-slot12 '%vk::descriptorBindingUpdateUnusedWhilePending)
							:descriptor-binding-partially-bound
							(feature-slot12 '%vk::descriptorBindingPartiallyBound)
							:descriptor-binding-variable-descriptor-count
							(feature-slot12 '%vk::descriptorBindingVariableDescriptorCount)
							:runtime-descriptor-array
							(feature-slot12 '%vk::runtimeDescriptorArray)
							:sampler-filter-minmax
							(feature-slot12 '%vk::samplerFilterMinmax)
							:scalar-block-layout
							(feature-slot12 '%vk::scalarBlockLayout)
							:imageless-framebuffer
							(feature-slot12 '%vk::imagelessFramebuffer)
							:uniform-buffer-standard-layout
							(feature-slot12 '%vk::uniformBufferStandardLayout)
							:shader-subgroup-extended-types
							(feature-slot12 '%vk::shaderSubgroupExtendedTypes)
							:separate-depth-stencil-layouts
							(feature-slot12 '%vk::separateDepthStencilLayouts)
							:host-query-reset
							(feature-slot12 '%vk::hostQueryReset)
							:timeline-semaphore
							(feature-slot12 '%vk::timelineSemaphore)
							:buffer-device-address
							(feature-slot12 '%vk::bufferDeviceAddress)
							:buffer-device-address-capture-replay
							(feature-slot12 '%vk::bufferDeviceAddressCaptureReplay)
							:buffer-device-address-multi-device
							(feature-slot12 '%vk::bufferDeviceAddressMultiDevice)
							:vulkan-memory-model
							(feature-slot12 '%vk::vulkanMemoryModel)
							:vulkan-memory-model-device-scope
							(feature-slot12 '%vk::vulkanMemoryModelDeviceScope)
							:vulkan-memory-model-availability-visibility-chains
							(feature-slot12 '%vk::vulkanMemoryModelAvailabilityVisibilityChains)
							:shader-output-viewport-index
							(feature-slot12 '%vk::shaderOutputViewportIndex)
							:shader-output-layer
							(feature-slot12 '%vk::shaderOutputLayer)
							:subgroup-broadcast-dynamic-id
							(feature-slot12 '%vk::subgroupBroadcastDynamicId)
					
							:max-image-dimension-1D
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxImageDimension1D)
							:max-image-dimension-2D
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxImageDimension2D)
							:max-image-dimension-3D
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxImageDimension3D)
							:max-image-dimension-cube
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxImageDimensionCube)
							:max-image-array-layers
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxImageArrayLayers)
							:max-texel-buffer-elements
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxTexelBufferElements)
							:max-uniform-buffer-range
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxUniformBufferRange)
							:max-storage-buffer-range
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxStorageBufferRange)
							:max-push-constants-size
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxPushConstantsSize)
							:max-memory-allocation-count
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxMemoryAllocationCount)
							:max-sampler-allocation-count
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxSamplerAllocationCount)
							:buffer-image-granularity
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::bufferImageGranularity)
							:sparse-address-space-size
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::sparseAddressSpaceSize)
							:max-bound-descriptor-sets
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxBoundDescriptorSets)
							:max-per-stage-descriptor-samplers
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxPerStageDescriptorSamplers)
							:max-per-stage-descriptor-uniform-buffers
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxPerStageDescriptorUniformBuffers)
							:max-per-stage-descriptor-storage-buffers
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxPerStageDescriptorStorageBuffers)
							:max-per-stage-descriptor-sampled-images
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxPerStageDescriptorSampledImages)
							:max-per-stage-descriptor-storage-images
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxPerStageDescriptorStorageImages)
							:max-per-stage-descriptor-input-attachments
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxPerStageDescriptorInputAttachments)
							:max-per-stage-resources
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxPerStageResources)
							:max-descriptor-set-samplers
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxDescriptorSetSamplers)
							:max-descriptor-set-uniform-buffers
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxDescriptorSetUniformBuffers)
							:max-descriptor-set-uniform-buffers-dynamic
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxDescriptorSetUniformBuffersDynamic)
							:max-descriptor-set-storage-buffers
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxDescriptorSetStorageBuffers)
							:max-descriptor-set-storage-buffers-dynamic
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxDescriptorSetStorageBuffersDynamic)
							:max-descriptor-set-sampled-images
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxDescriptorSetSampledImages)
							:max-descriptor-set-storage-images
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxDescriptorSetStorageImages)
							:max-descriptor-set-input-attachments
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxDescriptorSetInputAttachments)
							:max-vertex-input-attributes
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxVertexInputAttributes)
							:max-vertex-input-bindings
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxVertexInputBindings)
							:max-vertex-input-attribute-offset
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxVertexInputAttributeOffset)
							:max-vertex-input-binding-stride
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxVertexInputBindingStride)
							:max-vertex-output-components
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxVertexOutputComponents)
							:max-tessellation-generation-level
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxTessellationGenerationLevel)
							:max-tessellation-patch-size
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxTessellationPatchSize)
							:max-tessellation-control-per-vertex-input-components
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxTessellationControlPerVertexInputComponents)
							:max-tessellation-control-per-vertex-output-components
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxTessellationControlPerVertexOutputComponents)
							:max-tessellation-control-per-patch-output-components
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxTessellationControlPerPatchOutputComponents)
							:max-tessellation-control-total-output-components
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxTessellationControlTotalOutputComponents)
							:max-tessellation-evaluation-input-components
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxTessellationEvaluationInputComponents)
							:max-tessellation-evaluation-output-components
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxTessellationEvaluationOutputComponents)
							:max-geometry-shader-invocations
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxGeometryShaderInvocations)
							:max-geometry-input-components
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxGeometryInputComponents)
							:max-geometry-output-components
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxGeometryOutputComponents)
							:max-geometry-output-vertices
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxGeometryOutputVertices)
							:max-geometry-total-output-components
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxGeometryTotalOutputComponents)
							:max-fragment-input-components
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxFragmentInputComponents)
							:max-fragment-output-attachments
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxFragmentOutputAttachments)
							:max-fragment-dual-src-attachments
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxFragmentDualSrcAttachments)
							:max-fragment-combined-output-resources
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxFragmentCombinedOutputResources)
							:max-compute-shared-memory-size
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxComputeSharedMemorySize)
							:max-compute-work-group-count
							(make-array 3 :element-type '(unsigned-byte 32)
								      :initial-contents
								      (let ((p (foreign-slot-pointer p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxComputeWorkGroupCount)))
									(list
									 (mem-aref p :unsigned-int 0) (mem-aref p :unsigned-int 1) (mem-aref p :unsigned-int 2))))			
							:max-compute-work-group-invocations
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxComputeWorkGroupInvocations)
							:max-compute-work-group-size
							(make-array 3 :element-type '(unsigned-byte 32)
								      :initial-contents
								      (list
								       (mem-aref (foreign-slot-pointer p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxComputeWorkGroupSize)
										 :unsigned-int 0)
								       (mem-aref (foreign-slot-pointer p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxComputeWorkGroupSize)
										 :unsigned-int 1)
								       (mem-aref (foreign-slot-pointer p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxComputeWorkGroupSize)
										 :unsigned-int 2)))
							:sub-pixel-precision-bits
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::subPixelPrecisionBits)
							:sub-texel-precision-bits
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::subTexelPrecisionBits)
							:mipmap-precision-bits
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::mipmapPrecisionBits)
							:max-draw-indexed-index-value
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxDrawIndexedIndexValue)
							:max-draw-indirect-count
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxDrawIndirectCount)
							:max-sampler-lod-bias
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxSamplerLodBias)
							:max-sampler-anisotropy
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxSamplerAnisotropy)
							:max-viewports
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxViewports)
							:max-viewport-dimensions
							(make-array 2 :element-type '(unsigned-byte 32)
								      :initial-contents
								      (list (mem-aref (foreign-slot-pointer p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxViewportDimensions)
										      :unsigned-int 0)
									    (mem-aref (foreign-slot-pointer p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxViewportDimensions)
										      :unsigned-int 1)))
							:viewport-bounds-range
							(make-array 2 :element-type 'single-float
								      :initial-contents
								      (list (mem-aref (foreign-slot-pointer p-limits '(:struct VkPhysicalDeviceLimits) '%vk::viewportBoundsRange) :float 0)
									    (mem-aref (foreign-slot-pointer p-limits '(:struct VkPhysicalDeviceLimits) '%vk::viewportBoundsRange) :float 1)))
							:viewport-sub-pixel-bits
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::viewportSubPixelBits)
							:min-memory-map-alignment
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::minMemoryMapAlignment)
							:min-texel-buffer-offset-alignment
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::minTexelBufferOffsetAlignment)
							:min-uniform-buffer-offset-alignment
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::minUniformBufferOffsetAlignment)
							:min-storage-buffer-offset-alignment
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::minStorageBufferOffsetAlignment)
							:min-texel-offset
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::minTexelOffset)
							:max-texel-offset
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxTexelOffset)
							:min-texel-gather-offset
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::minTexelGatherOffset)
							:max-texel-gather-offset
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxTexelGatherOffset)
							:min-interpolation-offset
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::minInterpolationOffset)
							:max-interpolation-offset
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxInterpolationOffset)
							:sub-pixel-interpolation-offset-bits
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::subPixelInterpolationOffsetBits)
							:max-framebuffer-width
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxFramebufferWidth)
							:max-framebuffer-height
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxFramebufferHeight)
							:max-framebuffer-layers
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxFramebufferLayers)
							:framebuffer-color-sample-counts
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::framebufferColorSampleCounts)
							:framebuffer-depth-sample-counts
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::framebufferDepthSampleCounts)
							:framebuffer-stencil-sample-counts
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::framebufferStencilSampleCounts)
							:framebuffer-no-attachments-sample-counts
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::framebufferNoAttachmentsSampleCounts)
							:max-color-attachments
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxColorAttachments)
							:sampled-image-color-sample-counts
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::sampledImageColorSampleCounts)
							:sampled-image-integer-sample-counts
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::sampledImageIntegerSampleCounts)
							:sampled-image-depth-sample-counts
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::sampledImageDepthSampleCounts)
							:sampled-image-stencil-sample-counts
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::sampledImageStencilSampleCounts)
							:storage-image-sample-counts
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::storageImageSampleCounts)
							:max-sample-mask-words
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxSampleMaskWords)
							:timestamp-compute-and-graphics
							(unless (zerop (foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::timestampComputeAndGraphics))
							  t)
							:timestamp-period
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::timestampPeriod)
							:max-clip-distances
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxClipDistances)
							:max-cull-distances
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxCullDistances)
							:max-combined-clip-and-cull-distances
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::maxCombinedClipAndCullDistances)
							:discrete-queue-priorities
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::discreteQueuePriorities)
							:point-size-range
							(make-array 2 :element-type 'single-float
								      :initial-contents
								      (list (mem-aref (foreign-slot-pointer p-limits '(:struct VkPhysicalDeviceLimits) '%vk::pointSizeRange) :float 0)
									    (mem-aref (foreign-slot-pointer p-limits '(:struct VkPhysicalDeviceLimits) '%vk::pointSizeRange) :float 1)))
							:line-width-range
							(make-array 2 :element-type 'single-float
								      :initial-contents
								      (list (mem-aref (foreign-slot-pointer p-limits '(:struct VkPhysicalDeviceLimits) '%vk::lineWidthRange) :float 0)
									    (mem-aref (foreign-slot-pointer p-limits '(:struct VkPhysicalDeviceLimits) '%vk::lineWidthRange) :float 1)))
							:point-size-granularity
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::pointSizeGranularity)
							:line-width-granularity
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::lineWidthGranularity)
							:strict-lines
							(unless (zerop (foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::strictLines))
							  t)
							:standard-sample-locations
							(unless (zerop (foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::standardSampleLocations))
							  t)
							:optimal-buffer-copy-offset-alignment
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::optimalBufferCopyOffsetAlignment)
							:optimal-buffer-copy-row-pitch-alignment
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::optimalBufferCopyRowPitchAlignment)
							:non-coherent-atom-size
							(foreign-slot-value p-limits '(:struct VkPhysicalDeviceLimits) '%vk::nonCoherentAtomSize)
							)))
				   (setf (queue-families gpu) (get-physical-device-queue-family-properties gpu))
				   gpu))))))))))))))

(defun get-max-usable-sample-count (gpu)
  (let ((count (logand (slot-value gpu 'framebuffer-color-sample-counts)
		       (slot-value gpu 'framebuffer-depth-sample-counts))))
    (cond ((logtest count VK_SAMPLE_COUNT_64_BIT) VK_SAMPLE_COUNT_64_BIT)
	  ((logtest count VK_SAMPLE_COUNT_32_BIT) VK_SAMPLE_COUNT_32_BIT)
	  ((logtest count VK_SAMPLE_COUNT_16_BIT) VK_SAMPLE_COUNT_16_BIT)
	  ((logtest count VK_SAMPLE_COUNT_8_BIT) VK_SAMPLE_COUNT_8_BIT)
	  ((logtest count VK_SAMPLE_COUNT_4_BIT) VK_SAMPLE_COUNT_4_BIT)
	  ((logtest count VK_SAMPLE_COUNT_2_BIT) VK_SAMPLE_COUNT_2_BIT)
	  (t VK_SAMPLE_COUNT_1_BIT))))
	  
