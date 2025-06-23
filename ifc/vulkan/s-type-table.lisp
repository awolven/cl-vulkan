;; Copyright 2019 Andrew Kenneth Wolven <awolven@gmail.com>
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

(cl:in-package :%vk)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defparameter *s-type-table*
    (alexandria:plist-hash-table 
     (cl:list 'VkAcquireNextImageInfoKHX
	      VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHX

	      'VkPipelineRasterizationLineStateCreateInfoEXT
	      VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT

	      'VkPhysicalDeviceLineRasterizationFeaturesEXT
	      VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT
	      
	      'VkAndroidSurfaceCreateInfoKHR
	      VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR

	      'VkApplicationInfo
	      VK_STRUCTURE_TYPE_APPLICATION_INFO

	      'VkBindBufferMemoryInfoKHX
	      VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHX

	      'VkBindImageMemoryInfoKHX
	      VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHX

	      'VkBindImageMemorySwapchainInfoKHX
	      VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHX

	      'VkBindSparseInfo
	      VK_STRUCTURE_TYPE_BIND_SPARSE_INFO

	      'VkBufferCreateInfo
	      VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO

	      'VkBufferMemoryBarrier
	      VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER

	      'VkBufferViewCreateInfo
	      VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO

	      'VkCmdProcessCommandsInfoNVX
	      VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX

	      'VkCmdReserveSpaceForCommandsInfoNVX
	      VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX

	      'VkCommandBufferAllocateInfo
	      VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO

	      'VkCommandBufferBeginInfo
	      VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO

	      'VkCommandBufferInheritanceInfo
	      VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO

	      'VkCommandPoolCreateInfo
	      VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO

	      'VkComputePipelineCreateInfo
	      VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO

	      'VkCopyDescriptorSet
	      VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET

	      'VkD3D12FenceSubmitInfoKHX
	      VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHX

	      'VkDebugMarkerMarkerInfoEXT
	      VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT

	      'VkDebugMarkerObjectNameInfoEXT
	      VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT

	      'VkDebugMarkerObjectTagInfoEXT
	      VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT

	      'VkDebugReportCallbackCreateInfoEXT
	      VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT

	      'VkDedicatedAllocationBufferCreateInfoNV
	      VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV

	      'VkDedicatedAllocationImageCreateInfoNV
	      VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV

	      'VkDedicatedAllocationMemoryAllocateInfoNV
	      VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV

	      'VkDescriptorPoolCreateInfo
	      VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO

	      'VkDescriptorSetAllocateInfo
	      VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO

	      'VkDescriptorSetLayoutCreateInfo
	      VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO

	      'VkDescriptorUpdateTemplateCreateInfoKHR
	      VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR

	      'VkDeviceCreateInfo
	      VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO

	      'VkDeviceEventInfoEXT
	      VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT

	      'VkDeviceGeneratedCommandsFeaturesNVX
	      VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX

	      'VkDeviceGeneratedCommandsLimitsNVX
	      VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX

	      'VkDeviceGroupBindSparseInfoKHX
	      VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHX

	      'VkDeviceGroupCommandBufferBeginInfoKHX
	      VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHX

	      'VkDeviceGroupDeviceCreateInfoKHX
	      VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHX

	      'VkDeviceGroupPresentCapabilitiesKHX
	      VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHX

	      'VkDeviceGroupPresentInfoKHX
	      VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHX

	      'VkDeviceGroupRenderPassBeginInfoKHX
	      VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHX

	      'VkDeviceGroupSubmitInfoKHX
	      VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHX

	      'VkDeviceGroupSwapchainCreateInfoKHX
	      VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHX

	      'VkDeviceQueueCreateInfo
	      VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO

	      'VkDisplayEventInfoEXT
	      VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT

	      'VkDisplayModeCreateInfoKHR
	      VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR

	      'VkDisplayPowerInfoEXT
	      VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT

	      'VkDisplayPresentInfoKHR
	      VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR

	      'VkDisplaySurfaceCreateInfoKHR
	      VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR

	      'VkEventCreateInfo
	      VK_STRUCTURE_TYPE_EVENT_CREATE_INFO

	      'VkExportMemoryAllocateInfoKHX
	      VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHX

	      'VkExportMemoryAllocateInfoNV
	      VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV

	      'VkExportMemoryWin32HandleInfoKHX
	      VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHX

	      'VkExportMemoryWin32HandleInfoNV
	      VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV

	      'VkExportSemaphoreCreateInfoKHX
	      VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHX

	      'VkExportSemaphoreWin32HandleInfoKHX
	      VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHX

	      'VkExternalBufferPropertiesKHX
	      VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHX

	      'VkExternalImageFormatPropertiesKHX
	      VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHX

	      'VkExternalMemoryBufferCreateInfoKHX
	      VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHX

	      'VkExternalMemoryImageCreateInfoKHX
	      VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHX

	      'VkExternalMemoryImageCreateInfoNV
	      VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV

	      'VkExternalSemaphorePropertiesKHX
	      VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHX

	      'VkFenceCreateInfo
	      VK_STRUCTURE_TYPE_FENCE_CREATE_INFO

	      'VkFormatProperties2KHR
	      VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR

	      'VkFramebufferCreateInfo
	      VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO

	      'VkGraphicsPipelineCreateInfo
	      VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO

	      'VkHdrMetadataEXT
	      VK_STRUCTURE_TYPE_HDR_METADATA_EXT

	      'VkImageCreateInfo
	      VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO

	      'VkImageFormatProperties2KHR
	      VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR

	      'VkImageMemoryBarrier
	      VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER

	      'VkImageSwapchainCreateInfoKHX
	      VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHX

	      'VkImageViewCreateInfo
	      VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO

	      'VkImportMemoryFdInfoKHX
	      VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHX

	      'VkImportMemoryWin32HandleInfoKHX
	      VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHX

	      'VkImportMemoryWin32HandleInfoNV
	      VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV

	      'VkImportSemaphoreFdInfoKHX
	      VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHX

	      'VkImportSemaphoreWin32HandleInfoKHX
	      VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHX

	      'VkIndirectCommandsLayoutCreateInfoNVX
	      VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX

	      'VkInstanceCreateInfo
	      VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO

	      'VkIOSSurfaceCreateInfoMVK
	      VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK

	      VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO
	      VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO


	      ;;VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO

	      'VkMacOSSurfaceCreateInfoMVK
	      VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK

	      'VkMappedMemoryRange
	      VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE

	      'VkMemoryAllocateFlagsInfoKHX
	      VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHX

	      'VkMemoryAllocateFlagsInfo
	      VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO

	      'VkMemoryAllocateInfo
	      VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO

	      'VkMemoryBarrier
	      VK_STRUCTURE_TYPE_MEMORY_BARRIER

	      'VkMemoryFdPropertiesKHX
	      VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHX

	      'VkMemoryWin32HandlePropertiesKHX
	      VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHX

	      'VkMirSurfaceCreateInfoKHR
	      VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR

	      'VkObjectTableCreateInfoNVX
	      VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX

	      'VkPhysicalDeviceDiscardRectanglePropertiesEXT
	      VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT

	      'VkPhysicalDeviceExternalBufferInfoKHX
	      VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHX

	      'VkPhysicalDeviceExternalImageFormatInfoKHX
	      VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHX

	      'VkPhysicalDeviceExternalSemaphoreInfoKHX
	      VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHX

	      'VkPhysicalDeviceFeatures2KHR
	      VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR

	      'VkPhysicalDeviceGroupPropertiesKHX
	      VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHX

	      'VkPhysicalDeviceIDPropertiesKHX
	      VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHX

	      'VkPhysicalDeviceImageFormatInfo2KHR
	      VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR

	      'VkPhysicalDeviceMemoryProperties2KHR
	      VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR

	      'VkPhysicalDeviceMultiviewFeaturesKHX
	      VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHX

	      'VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
	      VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX

	      'VkPhysicalDeviceMultiviewPropertiesKHX
	      VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHX

	      'VkPhysicalDeviceProperties2KHR
	      VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR

	      'VkPhysicalDevicePushDescriptorPropertiesKHR
	      VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR

	      'VkPhysicalDeviceSparseImageFormatInfo2KHR
	      VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR

	      'VkPhysicalDeviceSurfaceInfo2KHR
	      VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR

	      'VkPipelineCacheCreateInfo
	      VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO

	      'VkPipelineColorBlendStateCreateInfo
	      VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO

	      'VkPipelineDepthStencilStateCreateInfo
	      VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO

	      'VkPipelineDiscardRectangleStateCreateInfoEXT
	      VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT

	      'VkPipelineDynamicStateCreateInfo
	      VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO

	      'VkPipelineInputAssemblyStateCreateInfo
	      VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO

	      'VkPipelineLayoutCreateInfo
	      VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO

	      'VkPipelineMultisampleStateCreateInfo
	      VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO

	      'VkPipelineRasterizationStateCreateInfo
	      VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO

	      'VkPipelineRasterizationStateRasterizationOrderAMD
	      VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD

	      'VkPipelineShaderStageCreateInfo
	      VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO

	      'VkPipelineTessellationStateCreateInfo
	      VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO

	      'VkPipelineVertexInputStateCreateInfo
	      VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO

	      'VkPipelineViewportStateCreateInfo
	      VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO

	      'VkPipelineViewportSwizzleStateCreateInfoNV
	      VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV

	      'VkPipelineViewportWScalingStateCreateInfoNV
	      VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV

	      'VkPresentInfoKHR
	      VK_STRUCTURE_TYPE_PRESENT_INFO_KHR

	      'VkPresentRegionsKHR
	      VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR

	      'VkPresentTimesInfoGOOGLE
	      VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE

	      'VkQueryPoolCreateInfo
	      VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO

	      'VkQueueFamilyProperties2KHR
	      VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR

	      'VkRenderPassBeginInfo
	      VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO

	      'VkRenderPassCreateInfo
	      VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO

	      'VkRenderPassMultiviewCreateInfoKHX
	      VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHX

	      'VkSamplerCreateInfo
	      VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO

	      'VkSemaphoreCreateInfo
	      VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO

	      'VkShaderModuleCreateInfo
	      VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO

	      'VkSharedPresentSurfaceCapabilitiesKHR
	      VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR

	      'VkSparseImageFormatProperties2KHR
	      VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR

	      'VkSubmitInfo
	      VK_STRUCTURE_TYPE_SUBMIT_INFO

	      'VkSurfaceCapabilities2EXT
	      VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT

	      'VkSurfaceCapabilities2KHR
	      VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR

	      'VkSurfaceFormat2KHR
	      VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR

	      'VkSwapchainCounterCreateInfoEXT
	      VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT

	      'VkSwapchainCreateInfoKHR
	      VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR

	      'VkTextureLODGatherFormatPropertiesAMD
	      VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD

	      'VkValidationFlagsEXT
	      VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT

	      'VkViSurfaceCreateInfoNN
	      VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN

	      'VkWaylandSurfaceCreateInfoKHR
	      VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR

	      'VkWin32KeyedMutexAcquireReleaseInfoKHX
	      VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHX

	      'VkWin32KeyedMutexAcquireReleaseInfoNV
	      VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV

	      'VkWin32SurfaceCreateInfoKHR
	      VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR

	      'VkWriteDescriptorSet
	      VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET

	      'VkXcbSurfaceCreateInfoKHR
	      VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR

	      'VkXlibSurfaceCreateInfoKHR
	      VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR))))
