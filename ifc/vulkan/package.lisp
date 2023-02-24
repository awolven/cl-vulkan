;; Copyright 2019, 2020 Andrew Kenneth Wolven
;; <awolven@gmail.com>
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

(cl:defpackage #:%vk
  (:use)
  (:nicknames #:vulkan-internals)
  (:export
   #:with-vk-struct
   #:zero-struct
   #:VK_TRUE
   #:VK_FALSE
   
   #:*s-type-table*
   #:|Display|
   #:xcb_connection_t
   #:MirSurface
   #:MirConnection
   #:ANativeWindow
   #:VkSampleMask
   #:VkBool32
   #:VkFlags
   #:VkDeviceSize
   ;;#:HANDLE
   #:SIZE-T
   #:NON-DISPATCH-HANDLE
   #:HINSTANCE
   #:HWND
   ;;#:HANDLE
   #:DWORD
   #:LPCWSTR
   #:RROutput
   #:xcb_window_t
   #:|window|
   #:VisualID
   #:SECURITY_ATTRIBUTES
   #:wl_display
   #:wl_surface
   #:VkInstance
   #:VkPhysicalDevice
   #:VkDevice
   #:VkQueue
   #:VkCommandBuffer
   #:VkDeviceMemory
   #:VkCommandPool
   #:VkBuffer
   #:VkBufferView
   #:VkImage
   #:VkImageView
   #:VkShaderModule
   #:VkPipeline
   #:VkPipelineLayout
   #:VkSampler
   #:VkDescriptorSet
   #:VkDescriptorSetLayout
   #:VkDescriptorPool
   #:VkFence
   #:VkSemaphore
   #:VkEvent
   #:VkQueryPool
   #:VkFramebuffer
   #:VkRenderPass
   #:VkPipelineCache
   #:VkObjectTableNVX
   #:VkIndirectCommandsLayoutNVX
   #:VkDescriptorUpdateTemplateKHR
   #:VkDisplayKHR
   #:VkDisplayModeKHR
   #:VkSurfaceKHR
   #:VkSwapchainKHR
   #:VkDebugReportCallbackEXT
   #:VkAccessFlagBits
   #:VkAccessFlags
   #:VkAndroidSurfaceCreateFlagsKHR
   #:VkAttachmentDescriptionFlagBits
   #:VkAttachmentDescriptionFlags
   #:VkBufferCreateFlagBits
   #:VkBufferCreateFlags
   #:VkBufferUsageFlagBits
   #:VkBufferUsageFlags
   #:VkBufferViewCreateFlags
   #:VkColorComponentFlagBits
   #:VkColorComponentFlags
   #:VkCommandBufferResetFlagBits
   #:VkCommandBufferResetFlags
   #:VkCommandBufferUsageFlagBits
   #:VkCommandBufferUsageFlags
   #:VkCommandPoolCreateFlagBits
   #:VkCommandPoolCreateFlags
   #:VkCommandPoolResetFlagBits
   #:VkCommandPoolResetFlags
   #:VkCommandPoolTrimFlagsKHR
   #:VkCompositeAlphaFlagBitsKHR
   #:VkCompositeAlphaFlagsKHR
   #:VkCullModeFlagBits
   #:VkCullModeFlags
   #:VkDebugReportFlagBitsEXT
   #:VkDebugReportFlagsEXT
   #:VkDependencyFlagBits
   #:VkDependencyFlags
   #:VkDescriptorPoolCreateFlagBits
   #:VkDescriptorPoolCreateFlags
   #:VkDescriptorPoolResetFlags
   #:VkDescriptorSetLayoutCreateFlagBits
   #:VkDescriptorSetLayoutCreateFlags
   #:VkDescriptorUpdateTemplateCreateFlagsKHR
   #:VkDeviceCreateFlags
   #:VkDeviceGroupPresentModeFlagBitsKHX
   #:VkDeviceGroupPresentModeFlagsKHX
   #:VkDeviceQueueCreateFlags
   #:VkDisplayModeCreateFlagsKHR
   #:VkDisplayPlaneAlphaFlagBitsKHR
   #:VkDisplayPlaneAlphaFlagsKHR
   #:VkDisplaySurfaceCreateFlagsKHR
   #:VkEventCreateFlags
   #:VkExternalMemoryFeatureFlagBitsKHX
   #:VkExternalMemoryFeatureFlagBitsNV
   #:VkExternalMemoryFeatureFlagsKHX
   #:VkExternalMemoryFeatureFlagsNV
   #:VkExternalMemoryHandleTypeFlagBitsKHX
   #:VkExternalMemoryHandleTypeFlagBitsNV
   #:VkExternalMemoryHandleTypeFlagsKHX
   #:VkExternalMemoryHandleTypeFlagsNV
   #:VkExternalSemaphoreFeatureFlagBitsKHX
   #:VkExternalSemaphoreFeatureFlagsKHX
   #:VkExternalSemaphoreHandleTypeFlagBitsKHX
   #:VkExternalSemaphoreHandleTypeFlagsKHX
   #:VkFenceCreateFlagBits
   #:VkFenceCreateFlags
   #:VkFormatFeatureFlagBits
   #:VkFormatFeatureFlags
   #:VkFramebufferCreateFlags
   #:VkIOSSurfaceCreateFlagsMVK
   #:VkImageAspectFlagBits
   #:VkImageAspectFlags
   #:VkImageCreateFlagBits
   #:VkImageCreateFlags
   #:VkImageUsageFlagBits
   #:VkImageUsageFlags
   #:VkImageViewCreateFlags
   #:VkIndirectCommandsLayoutUsageFlagBitsNVX
   #:VkIndirectCommandsLayoutUsageFlagsNVX
   #:VkInstanceCreateFlags
   #:VkMacOSSurfaceCreateFlagsMVK
   #:VkMemoryAllocateFlagBitsKHX
   #:VkMemoryAllocateFlagsKHX
   #:VkMemoryHeapFlagBits
   #:VkMemoryHeapFlags
   #:VkMemoryMapFlags
   #:VkMemoryPropertyFlagBits
   #:VkMemoryPropertyFlags
   #:VkMirSurfaceCreateFlagsKHR
   #:VkObjectEntryUsageFlagBitsNVX
   #:VkObjectEntryUsageFlagsNVX
   #:VkPeerMemoryFeatureFlagBitsKHX
   #:VkPeerMemoryFeatureFlagsKHX
   #:VkPipelineCacheCreateFlags
   #:VkPipelineColorBlendStateCreateFlags
   #:VkPipelineCreateFlagBits
   #:VkPipelineCreateFlags
   #:VkPipelineDepthStencilStateCreateFlags
   #:VkPipelineDiscardRectangleStateCreateFlagsEXT
   #:VkPipelineDynamicStateCreateFlags
   #:VkPipelineInputAssemblyStateCreateFlags
   #:VkPipelineLayoutCreateFlags
   #:VkPipelineMultisampleStateCreateFlags
   #:VkPipelineRasterizationStateCreateFlags
   #:VkPipelineShaderStageCreateFlags
   #:VkPipelineStageFlagBits
   #:VkPipelineStageFlags
   #:VkPipelineTessellationStateCreateFlags
   #:VkPipelineVertexInputStateCreateFlags
   #:VkPipelineViewportStateCreateFlags
   #:VkPipelineViewportSwizzleStateCreateFlagsNV
   #:VkQueryControlFlagBits
   #:VkQueryControlFlags
   #:VkQueryPipelineStatisticFlagBits
   #:VkQueryPipelineStatisticFlags
   #:VkQueryPoolCreateFlags
   #:VkQueryResultFlagBits
   #:VkQueryResultFlags
   #:VkQueueFlagBits
   #:VkQueueFlags
   #:VkRenderPassCreateFlags
   #:VkSampleCountFlagBits
   #:VkSampleCountFlags
   #:VkSamplerCreateFlags
   #:VkSemaphoreCreateFlags
   #:VkShaderModuleCreateFlags
   #:VkShaderStageFlagBits
   #:VkShaderStageFlags
   #:VkSparseImageFormatFlagBits
   #:VkSparseImageFormatFlags
   #:VkSparseMemoryBindFlagBits
   #:VkSparseMemoryBindFlags
   #:VkStencilFaceFlagBits
   #:VkStencilFaceFlags
   #:VkSubpassDescriptionFlagBits
   #:VkSubpassDescriptionFlags
   #:VkSurfaceCounterFlagBitsEXT
   #:VkSurfaceCounterFlagsEXT
   #:VkSurfaceTransformFlagBitsKHR
   #:VkSurfaceTransformFlagsKHR
   #:VkSwapchainCreateFlagBitsKHR
   #:VkSwapchainCreateFlagsKHR
   #:VkViSurfaceCreateFlagsNN
   #:VkWaylandSurfaceCreateFlagsKHR
   #:VkWin32SurfaceCreateFlagsKHR
   #:VkXcbSurfaceCreateFlagsKHR
   #:VkXlibSurfaceCreateFlagsKHR
   #:VkAccessFlagBits
   #:VkAttachmentDescriptionFlagBits
   #:VkAttachmentLoadOp
   #:VkAttachmentStoreOp
   #:VkBlendFactor
   #:VkBlendOp
   #:VkBorderColor
   #:VkBufferCreateFlagBits
   #:VkBufferUsageFlagBits
   #:VkColorComponentFlagBits
   #:VkColorSpaceKHR
   #:VkCommandBufferLevel
   #:VkCommandBufferResetFlagBits
   #:VkCommandBufferUsageFlagBits
   #:VkCommandPoolCreateFlagBits
   #:VkCommandPoolResetFlagBits
   #:VkCompareOp
   #:VkComponentSwizzle
   #:VkCompositeAlphaFlagBitsKHR
   #:VkCullModeFlagBits
   #:VkDebugReportFlagBitsEXT
   #:VkDebugReportObjectTypeEXT
   #:VkDependencyFlagBits
   #:VkDescriptorPoolCreateFlagBits
   #:VkDescriptorSetLayoutCreateFlagBits
   #:VkDescriptorType
   #:VkDescriptorUpdateTemplateTypeKHR
   #:VkDeviceEventTypeEXT
   #:VkDeviceGroupPresentModeFlagBitsKHX
   #:VkDiscardRectangleModeEXT
   #:VkDisplayEventTypeEXT
   #:VkDisplayPlaneAlphaFlagBitsKHR
   #:VkDisplayPowerStateEXT
   #:VkDynamicState
   #:VkExternalMemoryFeatureFlagBitsKHX
   #:VkExternalMemoryFeatureFlagBitsNV
   #:VkExternalMemoryHandleTypeFlagBitsKHX
   #:VkExternalMemoryHandleTypeFlagBitsNV
   #:VkExternalSemaphoreFeatureFlagBitsKHX
   #:VkExternalSemaphoreHandleTypeFlagBitsKHX
   #:VkFenceCreateFlagBits
   #:VkFilter
   #:VkFormat
   #:VkFormatFeatureFlagBits
   #:VkFrontFace
   #:VkImageAspectFlagBits
   #:VkImageCreateFlagBits
   #:VkImageLayout
   #:VkImageTiling
   #:VkImageType
   #:VkImageUsageFlagBits
   #:VkImageViewType
   #:VkIndexType
   #:VkIndirectCommandsLayoutUsageFlagBitsNVX
   #:VkIndirectCommandsTokenTypeNVX
   #:VkInternalAllocationType
   #:VkLogicOp
   #:VkMemoryAllocateFlagBitsKHX
   #:VkMemoryHeapFlagBits
   #:VkMemoryPropertyFlagBits
   #:VkObjectEntryTypeNVX
   #:VkObjectEntryUsageFlagBitsNVX
   #:VkObjectType
   #:VkPeerMemoryFeatureFlagBitsKHX
   #:VkPhysicalDeviceType
   #:VkPipelineBindPoint
   #:VkPipelineCacheHeaderVersion
   #:VkPipelineCreateFlagBits
   #:VkPipelineStageFlagBits
   #:VkPolygonMode
   #:VkPresentModeKHR
   #:VkPrimitiveTopology
   #:VkQueryControlFlagBits
   #:VkQueryPipelineStatisticFlagBits
   #:VkQueryResultFlagBits
   #:VkQueryType
   #:VkQueueFlagBits
   #:VkRasterizationOrderAMD
   #:VkResult
   #:VkSampleCountFlagBits
   #:VkSamplerAddressMode
   #:VkSamplerMipmapMode
   #:VkShaderStageFlagBits
   #:VkSharingMode
   #:VkSparseImageFormatFlagBits
   #:VkSparseMemoryBindFlagBits
   #:VkStencilFaceFlagBits
   #:VkStencilOp
   #:VkStructureType
   #:VkSubpassContents
   #:VkSubpassDescriptionFlagBits
   #:VkSurfaceCounterFlagBitsEXT
   #:VkSurfaceTransformFlagBitsKHR
   #:VkSwapchainCreateFlagBitsKHR
   #:VkSystemAllocationScope
   #:VkValidationCheckEXT
   #:VkVertexInputRate
   #:VkViewportCoordinateSwizzleNV
   #:PFN_vkAllocationFunction
   #:PFN_vkDebugReportCallbackEXT
   #:PFN_vkFreeFunction
   #:PFN_vkInternalAllocationNotification
   #:PFN_vkInternalFreeNotification
   #:PFN_vkReallocationFunction
   #:PFN_vkVoidFunction
   #:VkAcquireNextImageInfoKHX
   #:VkAllocationCallbacks
   #:VkAndroidSurfaceCreateInfoKHR
   #:VkApplicationInfo
   #:VkAttachmentDescription
   #:VkAttachmentReference
   #:VkBindBufferMemoryInfoKHX
   #:VkOffset2D
   #:VkExtent2D
   #:VkRect2D
   #:VkBindImageMemoryInfoKHX
   #:VkBindImageMemorySwapchainInfoKHX
   #:VkSparseMemoryBind
   #:VkSparseBufferMemoryBindInfo
   #:VkSparseImageOpaqueMemoryBindInfo
   #:VkImageSubresource
   #:VkOffset3D
   #:VkExtent3D
   #:VkSparseImageMemoryBind
   #:VkSparseImageMemoryBindInfo
   #:VkBindSparseInfo
   #:VkBufferCopy
   #:VkBufferCreateInfo
   #:VkImageSubresourceLayers
   #:VkBufferImageCopy
   #:VkBufferMemoryBarrier
   #:VkBufferViewCreateInfo
   #:VkClearColorValue
   #:VkClearDepthStencilValue
   #:VkClearValue
   #:VkClearAttachment
   #:VkClearRect
   #:VkIndirectCommandsTokenNVX
   #:VkCmdProcessCommandsInfoNVX
   #:VkCmdReserveSpaceForCommandsInfoNVX
   #:VkCommandBufferAllocateInfo
   #:VkCommandBufferInheritanceInfo
   #:VkCommandBufferBeginInfo
   #:VkCommandPoolCreateInfo
   #:VkComponentMapping
   #:VkSpecializationMapEntry
   #:VkSpecializationInfo
   #:VkPipelineShaderStageCreateInfo
   #:VkComputePipelineCreateInfo
   #:VkCopyDescriptorSet
   #:VkD3D12FenceSubmitInfoKHX
   #:VkDebugMarkerMarkerInfoEXT
   #:VkDebugMarkerObjectNameInfoEXT
   #:VkDebugMarkerObjectTagInfoEXT
   #:VkDebugReportCallbackCreateInfoEXT
   #:VkDedicatedAllocationBufferCreateInfoNV
   #:VkDedicatedAllocationImageCreateInfoNV
   #:VkDedicatedAllocationMemoryAllocateInfoNV
   #:VkDescriptorBufferInfo
   #:VkDescriptorImageInfo
   #:VkDescriptorPoolSize
   #:VkDescriptorPoolCreateInfo
   #:VkDescriptorSetAllocateInfo
   #:VkDescriptorSetLayoutBinding
   #:VkDescriptorSetLayoutCreateInfo
   #:VkDescriptorUpdateTemplateEntryKHR
   #:VkDescriptorUpdateTemplateCreateInfoKHR
   #:VkDeviceQueueCreateInfo
   #:VkPhysicalDeviceFeatures
   #:VkDeviceCreateInfo
   #:VkDeviceEventInfoEXT
   #:VkDeviceGeneratedCommandsFeaturesNVX
   #:VkDeviceGeneratedCommandsLimitsNVX
   #:VkDeviceGroupBindSparseInfoKHX
   #:VkDeviceGroupCommandBufferBeginInfoKHX
   #:VkDeviceGroupDeviceCreateInfoKHX
   #:VkDeviceGroupPresentCapabilitiesKHX
   #:VkDeviceGroupPresentInfoKHX
   #:VkDeviceGroupRenderPassBeginInfoKHX
   #:VkDeviceGroupSubmitInfoKHX
   #:VkDeviceGroupSwapchainCreateInfoKHX
   #:VkDispatchIndirectCommand
   #:VkDisplayEventInfoEXT
   #:VkDisplayModeParametersKHR
   #:VkDisplayModeCreateInfoKHR
   #:VkDisplayModePropertiesKHR
   #:VkDisplayPlaneCapabilitiesKHR
   #:VkDisplayPlanePropertiesKHR
   #:VkDisplayPowerInfoEXT
   #:VkDisplayPresentInfoKHR
   #:VkDisplayPropertiesKHR
   #:VkDisplaySurfaceCreateInfoKHR
   #:VkDrawIndexedIndirectCommand
   #:VkDrawIndirectCommand
   #:VkEventCreateInfo
   #:VkExportMemoryAllocateInfoKHX
   #:VkExportMemoryAllocateInfoNV
   #:VkExportMemoryWin32HandleInfoKHX
   #:VkExportMemoryWin32HandleInfoNV
   #:VkExportSemaphoreCreateInfoKHX
   #:VkExportSemaphoreWin32HandleInfoKHX
   #:VkExtensionProperties
   #:VkExternalMemoryPropertiesKHX
   #:VkExternalBufferPropertiesKHX
   #:VkExternalImageFormatPropertiesKHX
   #:VkImageFormatProperties
   #:VkExternalImageFormatPropertiesNV
   #:VkExternalMemoryBufferCreateInfoKHX
   #:VkExternalMemoryImageCreateInfoKHX
   #:VkExternalMemoryImageCreateInfoNV
   #:VkExternalSemaphorePropertiesKHX
   #:VkFenceCreateInfo
   #:VkFormatProperties
   #:VkFormatProperties2KHR
   #:VkFramebufferCreateInfo
   #:VkVertexInputBindingDescription
   #:VkVertexInputAttributeDescription
   #:VkPipelineVertexInputStateCreateInfo
   #:VkPipelineInputAssemblyStateCreateInfo
   #:VkPipelineTessellationStateCreateInfo
   #:VkViewport
   #:VkPipelineViewportStateCreateInfo
   #:VkPipelineRasterizationLineStateCreateInfoEXT
   #:VkPipelineRasterizationStateCreateInfo
   #:VkPipelineMultisampleStateCreateInfo
   #:VkStencilOpState
   #:VkPipelineDepthStencilStateCreateInfo
   #:VkPipelineColorBlendAttachmentState
   #:VkPipelineColorBlendStateCreateInfo
   #:VkPipelineDynamicStateCreateInfo
   #:VkGraphicsPipelineCreateInfo
   #:VkXYColorEXT
   #:VkHdrMetadataEXT
   #:VkIOSSurfaceCreateInfoMVK
   #:VkImageBlit
   #:VkImageCopy
   #:VkImageCreateInfo
   #:VkImageFormatProperties2KHR
   #:VkImageSubresourceRange
   #:VkImageMemoryBarrier
   #:VkImageResolve
   #:VkImageSwapchainCreateInfoKHX
   #:VkImageViewCreateInfo
   #:VkImportMemoryFdInfoKHX
   #:VkImportMemoryWin32HandleInfoKHX
   #:VkImportMemoryWin32HandleInfoNV
   #:VkImportSemaphoreFdInfoKHX
   #:VkImportSemaphoreWin32HandleInfoKHX
   #:VkIndirectCommandsLayoutTokenNVX
   #:VkIndirectCommandsLayoutCreateInfoNVX
   #:VkInstanceCreateInfo
   #:VkLayerProperties
   #:VkMacOSSurfaceCreateInfoMVK
   #:VkMappedMemoryRange
   #:VkMemoryAllocateFlagsInfoKHX
   #:VkMemoryAllocateInfo
   #:VkMemoryBarrier
   #:VkMemoryFdPropertiesKHX
   #:VkMemoryHeap
   #:VkMemoryRequirements
   #:VkMemoryType
   #:VkMemoryWin32HandlePropertiesKHX
   #:VkMirSurfaceCreateInfoKHR
   #:VkObjectTableCreateInfoNVX
   #:VkObjectTableDescriptorSetEntryNVX
   #:VkObjectTableEntryNVX
   #:VkObjectTableIndexBufferEntryNVX
   #:VkObjectTablePipelineEntryNVX
   #:VkObjectTablePushConstantEntryNVX
   #:VkObjectTableVertexBufferEntryNVX
   #:VkPastPresentationTimingGOOGLE
   #:VkPhysicalDeviceDiscardRectanglePropertiesEXT
   #:VkPhysicalDeviceExternalBufferInfoKHX
   #:VkPhysicalDeviceExternalImageFormatInfoKHX
   #:VkPhysicalDeviceExternalSemaphoreInfoKHX
   #:VkPhysicalDeviceFeatures2KHR
   #:VkPhysicalDeviceGroupPropertiesKHX
   #:VkPhysicalDeviceIDPropertiesKHX
   #:VkPhysicalDeviceImageFormatInfo2KHR
   #:VkPhysicalDeviceLimits
   #:VkPhysicalDeviceMemoryProperties
   #:VkPhysicalDeviceMemoryProperties2KHR
   #:VkPhysicalDeviceMultiviewFeaturesKHX
   #:VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
   #:VkPhysicalDeviceMultiviewPropertiesKHX
   #:VkPhysicalDeviceSparseProperties
   #:VkPhysicalDeviceProperties
   #:VkPhysicalDeviceProperties2KHR
   #:VkPhysicalDevicePushDescriptorPropertiesKHR
   #:VkPhysicalDeviceSparseImageFormatInfo2KHR
   #:VkPhysicalDeviceSurfaceInfo2KHR
   #:VkPipelineCacheCreateInfo
   #:VkPipelineDiscardRectangleStateCreateInfoEXT
   #:VkPushConstantRange
   #:VkPipelineLayoutCreateInfo
   #:VkPipelineRasterizationStateRasterizationOrderAMD
   #:VkViewportSwizzleNV
   #:VkPipelineViewportSwizzleStateCreateInfoNV
   #:VkViewportWScalingNV
   #:VkPipelineViewportWScalingStateCreateInfoNV
   #:VkPresentInfoKHR
   #:VkRectLayerKHR
   #:VkPresentRegionKHR
   #:VkPresentRegionsKHR
   #:VkPresentTimeGOOGLE
   #:VkPresentTimesInfoGOOGLE
   #:VkQueryPoolCreateInfo
   #:VkQueueFamilyProperties
   #:VkQueueFamilyProperties2KHR
   #:VkRect3D
   #:VkRefreshCycleDurationGOOGLE
   #:VkRenderPassBeginInfo
   #:VkSubpassDescription
   #:VkSubpassDependency
   #:VkRenderPassCreateInfo
   #:VkRenderPassMultiviewCreateInfoKHX
   #:VkSamplerCreateInfo
   #:VkSemaphoreCreateInfo
   #:VkShaderModuleCreateInfo
   #:VkSharedPresentSurfaceCapabilitiesKHR
   #:VkSparseImageFormatProperties
   #:VkSparseImageFormatProperties2KHR
   #:VkSparseImageMemoryRequirements
   #:VkSubmitInfo
   #:VkSubresourceLayout
   #:VkSurfaceCapabilities2EXT
   #:VkSurfaceCapabilitiesKHR
   #:VkSurfaceCapabilities2KHR
   #:VkSurfaceFormatKHR
   #:VkSurfaceFormat2KHR
   #:VkSwapchainCounterCreateInfoEXT
   #:VkSwapchainCreateInfoKHR
   #:VkTextureLODGatherFormatPropertiesAMD
   #:VkValidationFlagsEXT
   #:VkViSurfaceCreateInfoNN
   #:VkWaylandSurfaceCreateInfoKHR
   #:VkWin32KeyedMutexAcquireReleaseInfoKHX
   #:VkWin32KeyedMutexAcquireReleaseInfoNV
   #:VkWin32SurfaceCreateInfoKHR
   #:VkWriteDescriptorSet
   #:VkXcbSurfaceCreateInfoKHR
   #:VkXlibSurfaceCreateInfoKHR
    
   #:PFN_vkAllocationFunction		  ;; FUNC
   #:PFN_vkDebugReportCallbackEXT	  ;; FUNC
   #:PFN_vkFreeFunction			  ;; FUNC
   #:PFN_vkInternalAllocationNotification ;; FUNC
   #:PFN_vkInternalFreeNotification	  ;; FUNC
   #:PFN_vkReallocationFunction		  ;; FUNC
   #:PFN_vkVoidFunction			  ;; FUNC
   
   #:VkAccessFlagBits			       ;; ENUM
   #:VkAccessFlags			       ;; BITMASK
   #:VkAcquireNextImageInfoKHX		       ;; :struct
   #:VkAllocationCallbacks		       ;; :struct
   #:VkAndroidSurfaceCreateFlagsKHR	       ;; BITMASK
   #:VkAndroidSurfaceCreateInfoKHR	       ;; :struct
   #:VkApplicationInfo			       ;; :struct
   #:VkAttachmentDescription		       ;; :struct
   #:VkAttachmentDescriptionFlagBits	       ;; ENUM
   #:VkAttachmentDescriptionFlags	       ;; BITMASK
   #:VkAttachmentLoadOp			       ;; ENUM
   #:VkAttachmentReference		       ;; :struct
   #:VkAttachmentStoreOp		       ;; ENUM
   #:VkBindBufferMemoryInfoKHX		       ;; :struct
   #:VkBindImageMemoryInfoKHX		       ;; :struct
   #:VkBindImageMemorySwapchainInfoKHX	       ;; :struct
   #:VkBindSparseInfo			       ;; :struct
   #:VkBlendFactor			       ;; ENUM
   #:VkBlendOp				       ;; ENUM
   #:VkBool32				       ;; ALIAS
   #:VkBorderColor			       ;; ENUM
   #:VkBuffer				       ;; NON-DISPATCH-HANDLE
   #:VkBufferCopy			       ;; :struct
   #:VkBufferCreateFlagBits		       ;; ENUM
   #:VkBufferCreateFlags		       ;; BITMASK
   #:VkBufferCreateInfo			       ;; :struct
   #:VkBufferImageCopy			       ;; :struct
   #:VkBufferMemoryBarrier		       ;; :struct
   #:VkBufferUsageFlagBits		       ;; ENUM
   #:VkBufferUsageFlags			       ;; BITMASK
   #:VkBufferView			       ;; NON-DISPATCH-HANDLE
   #:VkBufferViewCreateFlagBits		       ;; ENUM
   #:VkBufferViewCreateFlags		       ;; BITMASK
   #:VkBufferViewCreateInfo		       ;; :struct
   #:VkClearAttachment			       ;; :struct
   #:VkClearColorValue			       ;; :union
   #:VkClearDepthStencilValue		       ;; :struct
   #:VkClearRect			       ;; :struct
   #:VkClearValue			       ;; :union
   #:VkCmdProcessCommandsInfoNVX	       ;; :struct
   #:VkCmdReserveSpaceForCommandsInfoNVX       ;; :struct
   #:VkColorComponentFlagBits		       ;; ENUM
   #:VkColorComponentFlags		       ;; BITMASK
   #:VkColorSpaceKHR			       ;; ENUM
   #:VkCommandBuffer			       ;; HANDLE
   #:VkCommandBufferAllocateInfo	       ;; :struct
   #:VkCommandBufferBeginInfo		       ;; :struct
   #:VkCommandBufferInheritanceInfo	       ;; :struct
   #:VkCommandBufferLevel		       ;; ENUM
   #:VkCommandBufferResetFlagBits	       ;; ENUM
   #:VkCommandBufferResetFlags		       ;; BITMASK
   #:VkCommandBufferUsageFlagBits	       ;; ENUM
   #:VkCommandBufferUsageFlags		       ;; BITMASK
   #:VkCommandPool			       ;; NON-DISPATCH-HANDLE
   #:VkCommandPoolCreateFlagBits	       ;; ENUM
   #:VkCommandPoolCreateFlags		       ;; BITMASK
   #:VkCommandPoolCreateInfo		       ;; :struct
   #:VkCommandPoolResetFlagBits		       ;; ENUM
   #:VkCommandPoolResetFlags		       ;; BITMASK
   #:VkCommandPoolTrimFlagsKHR		       ;; BITMASK
   #:VkCompareOp			       ;; ENUM
   #:VkComponentMapping			       ;; :struct
   #:VkComponentSwizzle			       ;; ENUM
   #:VkCompositeAlphaFlagBitsKHR	       ;; ENUM
   #:VkCompositeAlphaFlagsKHR		       ;; BITMASK
   #:VkComputePipelineCreateInfo	       ;; :struct
   #:VkCopyDescriptorSet		       ;; :struct
   #:VkCullModeFlagBits			       ;; ENUM
   #:VkCullModeFlags			       ;; BITMASK
   #:VkD3D12FenceSubmitInfoKHX		       ;; :struct
   #:VkDebugMarkerMarkerInfoEXT		       ;; :struct
   #:VkDebugMarkerObjectNameInfoEXT	       ;; :struct
   #:VkDebugMarkerObjectTagInfoEXT	       ;; :struct
   #:VkDebugReportCallbackCreateInfoEXT	       ;; :struct
   #:VkDebugReportCallbackEXT		       ;; NON-DISPATCH-HANDLE
   #:VkDebugReportFlagBitsEXT		       ;; ENUM
   #:VkDebugReportFlagsEXT		       ;; BITMASK
   #:VkDebugReportObjectTypeEXT		       ;; ENUM
   #:VkDedicatedAllocationBufferCreateInfoNV   ;; :struct
   #:VkDedicatedAllocationImageCreateInfoNV    ;; :struct
   #:VkDedicatedAllocationMemoryAllocateInfoNV ;; :struct
   #:VkDependencyFlagBits		       ;; ENUM
   #:VkDependencyFlags			       ;; BITMASK
   #:VkDescriptorBufferInfo		       ;; :struct
   #:VkDescriptorImageInfo		       ;; :struct
   #:VkDescriptorPool			       ;; NON-DISPATCH-HANDLE
   #:VkDescriptorPoolCreateFlagBits	       ;; ENUM
   #:VkDescriptorPoolCreateFlags	       ;; BITMASK
   #:VkDescriptorPoolCreateInfo		       ;; :struct
   #:VkDescriptorPoolResetFlags		       ;; BITMASK
   #:VkDescriptorPoolSize		       ;; :struct
   #:VkDescriptorSet			       ;; NON-DISPATCH-HANDLE
   #:VkDescriptorSetAllocateInfo	       ;; :struct
   #:VkDescriptorSetLayout		       ;; NON-DISPATCH-HANDLE
   #:VkDescriptorSetLayoutBinding	       ;; :struct
   #:VkDescriptorSetLayoutCreateFlagBits       ;; ENUM
   #:VkDescriptorSetLayoutCreateFlags	       ;; BITMASK
   #:VkDescriptorSetLayoutCreateInfo	       ;; :struct
   #:VkDescriptorType			       ;; ENUM
   #:VkDescriptorUpdateTemplateCreateFlagsKHR  ;; BITMASK
   #:VkDescriptorUpdateTemplateCreateInfoKHR   ;; :struct
   #:VkDescriptorUpdateTemplateEntryKHR	       ;; :struct
   #:VkDescriptorUpdateTemplateKHR	       ;; NON-DISPATCH-HANDLE
   #:VkDescriptorUpdateTemplateTypeKHR	       ;; ENUM
   #:VkDevice				       ;; HANDLE
   #:VkDeviceCreateFlagBits		       ;; ENUM
   #:VkDeviceCreateFlags		       ;; BITMASK
   #:VkDeviceCreateInfo			       ;; :struct
   #:VkDeviceEventInfoEXT		       ;; :struct
   #:VkDeviceEventTypeEXT		       ;; ENUM
   #:VkDeviceGeneratedCommandsFeaturesNVX      ;; :struct
   #:VkDeviceGeneratedCommandsLimitsNVX	       ;; :struct
   #:VkDeviceGroupBindSparseInfoKHX	       ;; :struct
   #:VkDeviceGroupCommandBufferBeginInfoKHX    ;; :struct
   #:VkDeviceGroupDeviceCreateInfoKHX	       ;; :struct
   #:VkDeviceGroupPresentCapabilitiesKHX       ;; :struct
   #:VkDeviceGroupPresentInfoKHX	       ;; :struct
   #:VkDeviceGroupPresentModeFlagBitsKHX       ;; ENUM
   #:VkDeviceGroupPresentModeFlagsKHX	       ;; BITMASK
   #:VkDeviceGroupRenderPassBeginInfoKHX       ;; :struct
   #:VkDeviceGroupSubmitInfoKHX		       ;; :struct
   #:VkDeviceGroupSwapchainCreateInfoKHX       ;; :struct
   #:VkDeviceMemory			       ;; NON-DISPATCH-HANDLE
   #:VkDeviceQueueCreateFlagBits	       ;; ENUM
   #:VkDeviceQueueCreateFlags		       ;; BITMASK
   #:VkDeviceQueueCreateInfo		       ;; :struct
   #:VkDeviceSize			       ;; ALIAS
   #:VkDiscardRectangleModeEXT		       ;; ENUM
   #:VkDispatchIndirectCommand		       ;; :struct
   #:VkDisplayEventInfoEXT		       ;; :struct
   #:VkDisplayEventTypeEXT		       ;; ENUM
   #:VkDisplayKHR			       ;; NON-DISPATCH-HANDLE
   #:VkDisplayModeCreateFlagsKHR	       ;; BITMASK
   #:VkDisplayModeCreateInfoKHR		       ;; :struct
   #:VkDisplayModeKHR			       ;; NON-DISPATCH-HANDLE
   #:VkDisplayModeParametersKHR		       ;; :struct
   #:VkDisplayModePropertiesKHR		       ;; :struct
   #:VkDisplayPlaneAlphaFlagBitsKHR	       ;; ENUM
   #:VkDisplayPlaneAlphaFlagsKHR	       ;; BITMASK
   #:VkDisplayPlaneCapabilitiesKHR	       ;; :struct
   #:VkDisplayPlanePropertiesKHR	       ;; :struct
   #:VkDisplayPowerInfoEXT		       ;; :struct
   #:VkDisplayPowerStateEXT		       ;; ENUM
   #:VkDisplayPresentInfoKHR		       ;; :struct
   #:VkDisplayPropertiesKHR		       ;; :struct
   #:VkDisplaySurfaceCreateFlagsKHR	       ;; BITMASK
   #:VkDisplaySurfaceCreateInfoKHR	       ;; :struct
   #:VkDrawIndexedIndirectCommand	       ;; :struct
   #:VkDrawIndirectCommand		       ;; :struct
   #:VkDynamicState			       ;; ENUM
   #:VkEvent				       ;; NON-DISPATCH-HANDLE
   #:VkEventCreateFlags			       ;; BITMASK
   #:VkEventCreateInfo			       ;; :struct
   #:VkExportMemoryAllocateInfoKHX	       ;; :struct
   #:VkExportMemoryAllocateInfoNV	       ;; :struct
   #:VkExportMemoryWin32HandleInfoKHX	       ;; :struct
   #:VkExportMemoryWin32HandleInfoNV	       ;; :struct
   #:VkExportSemaphoreCreateInfoKHX	       ;; :struct
   #:VkExportSemaphoreWin32HandleInfoKHX       ;; :struct
   #:VkExtensionProperties		       ;; :struct
   #:VkExtent2D				       ;; :struct
   #:VkExtent3D				       ;; :struct
   #:VkExternalBufferPropertiesKHX	       ;; :struct
   #:VkExternalImageFormatPropertiesKHX	       ;; :struct
   #:VkExternalImageFormatPropertiesNV	       ;; :struct
   #:VkExternalMemoryBufferCreateInfoKHX       ;; :struct
   #:VkExternalMemoryFeatureFlagBitsKHX	       ;; ENUM
   #:VkExternalMemoryFeatureFlagBitsNV	       ;; ENUM
   #:VkExternalMemoryFeatureFlagsKHX	       ;; BITMASK
   #:VkExternalMemoryFeatureFlagsNV	       ;; BITMASK
   #:VkExternalMemoryHandleTypeFlagBitsKHX     ;; ENUM
   #:VkExternalMemoryHandleTypeFlagBitsNV      ;; ENUM
   #:VkExternalMemoryHandleTypeFlagsKHX	       ;; BITMASK
   #:VkExternalMemoryHandleTypeFlagsNV	       ;; BITMASK
   #:VkExternalMemoryImageCreateInfoKHX	       ;; :struct
   #:VkExternalMemoryImageCreateInfoNV	       ;; :struct
   #:VkExternalMemoryPropertiesKHX	       ;; :struct
   #:VkExternalSemaphoreFeatureFlagBitsKHX     ;; ENUM
   #:VkExternalSemaphoreFeatureFlagsKHX	       ;; BITMASK
   #:VkExternalSemaphoreHandleTypeFlagBitsKHX  ;; ENUM
   #:VkExternalSemaphoreHandleTypeFlagsKHX     ;; BITMASK
   #:VkExternalSemaphorePropertiesKHX	       ;; :struct
   #:VkFence				       ;; NON-DISPATCH-HANDLE
   #:VkFenceCreateFlagBits		       ;; ENUM
   #:VkFenceCreateFlags			       ;; BITMASK
   #:VkFenceCreateInfo			       ;; :struct
   #:VkFilter				       ;; ENUM
   #:VkFlags				       ;; ALIAS
   #:VkFormat				       ;; ENUM
   #:VkFormatFeatureFlagBits		       ;; ENUM
   #:VkFormatFeatureFlags		       ;; BITMASK
   #:VkFormatProperties			       ;; :struct
   #:VkFormatProperties2KHR		       ;; :struct
   #:VkFramebuffer			       ;; NON-DISPATCH-HANDLE
   #:VkFramebufferCreateFlagBits	       ;; ENUM
   #:VkFramebufferCreateFlags		       ;; BITMASK
   #:VkFramebufferCreateInfo		       ;; :struct
   #:VkFrontFace			       ;; ENUM
   #:VkGraphicsPipelineCreateInfo	       ;; :struct
   #:VkHdrMetadataEXT			       ;; :struct
   #:VkIOSSurfaceCreateFlagsMVK		       ;; BITMASK
   #:VkIOSSurfaceCreateInfoMVK		       ;; :struct
   #:VkImage				       ;; NON-DISPATCH-HANDLE
   #:VkImageAspectFlagBits		       ;; ENUM
   #:VkImageAspectFlags			       ;; BITMASK
   #:VkImageBlit			       ;; :struct
   #:VkImageCopy			       ;; :struct
   #:VkImageCreateFlagBits		       ;; ENUM
   #:VkImageCreateFlags			       ;; BITMASK
   #:VkImageCreateInfo			       ;; :struct
   #:VkImageFormatProperties		       ;; :struct
   #:VkImageFormatProperties2KHR	       ;; :struct
   #:VkImageLayout			       ;; ENUM
   #:VkImageMemoryBarrier		       ;; :struct
   #:VkImageResolve			       ;; :struct
   #:VkImageSubresource			       ;; :struct
   #:VkImageSubresourceLayers		       ;; :struct
   #:VkImageSubresourceRange		       ;; :struct
   #:VkImageSwapchainCreateInfoKHX	       ;; :struct
   #:VkImageTiling			       ;; ENUM
   #:VkImageType			       ;; ENUM
   #:VkImageUsageFlagBits		       ;; ENUM
   #:VkImageUsageFlags			       ;; BITMASK
   #:VkImageView			       ;; NON-DISPATCH-HANDLE
   #:VkImageViewCreateFlags		       ;; BITMASK
   #:VkImageViewCreateInfo		       ;; :struct
   #:VkImageViewType			       ;; ENUM
   #:VkImportMemoryFdInfoKHX		       ;; :struct
   #:VkImportMemoryWin32HandleInfoKHX	       ;; :struct
   #:VkImportMemoryWin32HandleInfoNV	       ;; :struct
   #:VkImportSemaphoreFdInfoKHX		       ;; :struct
   #:VkImportSemaphoreWin32HandleInfoKHX       ;; :struct
   #:VkIndexType			       ;; ENUM
   #:VkIndirectCommandsLayoutCreateInfoNVX     ;; :struct
   #:VkIndirectCommandsLayoutNVX	       ;; NON-DISPATCH-HANDLE
   #:VkIndirectCommandsLayoutTokenNVX	       ;; :struct
   #:VkIndirectCommandsLayoutUsageFlagBitsNVX  ;; ENUM
   #:VkIndirectCommandsLayoutUsageFlagsNVX     ;; BITMASK
   #:VkIndirectCommandsTokenNVX		       ;; :struct
   #:VkIndirectCommandsTokenTypeNVX	       ;; ENUM
   #:VkInstance				       ;; HANDLE
   #:VkInstanceCreateFlagBits		       ;; ENUM
   #:VkInstanceCreateFlags		       ;; BITMASK
   #:VkInstanceCreateInfo		       ;; :struct
   #:VkInternalAllocationType		       ;; ENUM
   #:VkLayerProperties			       ;; :struct
   #:VkLogicOp				       ;; ENUM
   #:VkMacOSSurfaceCreateFlagsMVK	       ;; BITMASK
   #:VkMacOSSurfaceCreateInfoMVK	       ;; :struct
   #:VkMappedMemoryRange		       ;; :struct
   #:VkMemoryAllocateFlagBitsKHX	       ;; ENUM
   #:VkMemoryAllocateFlagsInfoKHX	       ;; :struct
   #:VkMemoryAllocateFlagsKHX		       ;; BITMASK
   #:VkMemoryAllocateInfo		       ;; :struct
   #:VkMemoryBarrier			       ;; :struct
   #:VkMemoryFdPropertiesKHX		       ;; :struct
   #:VkMemoryHeap			       ;; :struct
   #:VkMemoryHeapFlagBits		       ;; ENUM
   #:VkMemoryHeapFlags			       ;; BITMASK
   #:VkMemoryMapFlags			       ;; BITMASK
   #:VkMemoryPropertyFlagBits		       ;; ENUM
   #:VkMemoryPropertyFlags		       ;; BITMASK
   #:VkMemoryRequirements		       ;; :struct
   #:VkMemoryType			       ;; :struct
   #:VkMemoryWin32HandlePropertiesKHX	       ;; :struct
   #:VkMirSurfaceCreateFlagsKHR		       ;; BITMASK
   #:VkMirSurfaceCreateInfoKHR		       ;; :struct
   #:VkObjectEntryTypeNVX		       ;; ENUM
   #:VkObjectEntryUsageFlagBitsNVX	       ;; ENUM
   #:VkObjectEntryUsageFlagsNVX		       ;; BITMASK
   #:VkObjectTableCreateInfoNVX		       ;; :struct
   #:VkObjectTableDescriptorSetEntryNVX	       ;; :struct
   #:VkObjectTableEntryNVX		       ;; :struct
   #:VkObjectTableIndexBufferEntryNVX	       ;; :struct
   #:VkObjectTableNVX			       ;; NON-DISPATCH-HANDLE
   #:VkObjectTablePipelineEntryNVX	       ;; :struct
   #:VkObjectTablePushConstantEntryNVX	       ;; :struct
   #:VkObjectTableVertexBufferEntryNVX	       ;; :struct
   #:VkObjectType			       ;; ENUM
   #:VkOffset2D				       ;; :struct
   #:VkOffset3D				       ;; :struct
   #:VkPastPresentationTimingGOOGLE	       ;; :struct
   #:VkPeerMemoryFeatureFlagBitsKHX	       ;; ENUM
   #:VkPeerMemoryFeatureFlagsKHX	       ;; BITMASK
   #:VkPhysicalDevice			       ;; HANDLE
   #:VkPhysicalDeviceDiscardRectanglePropertiesEXT ;; :struct
   #:VkPhysicalDeviceExternalBufferInfoKHX	   ;; :struct
   #:VkPhysicalDeviceExternalImageFormatInfoKHX	   ;; :struct
   #:VkPhysicalDeviceExternalSemaphoreInfoKHX	   ;; :struct
   #:VkPhysicalDeviceFeatures			   ;; :struct
   #:VkPhysicalDeviceFeatures2KHR		   ;; :struct
   #:VkPhysicalDeviceGroupPropertiesKHX		   ;; :struct
   #:VkPhysicalDeviceIDPropertiesKHX		   ;; :struct
   #:VkPhysicalDeviceImageFormatInfo2KHR	   ;; :struct
   #:VkPhysicalDeviceLimits			   ;; :struct
   #:VkPhysicalDeviceMemoryProperties		   ;; :struct
   #:VkPhysicalDeviceMemoryProperties2KHR	   ;; :struct
   #:VkPhysicalDeviceMultiviewFeaturesKHX	   ;; :struct
   #:VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX ;; :struct
   #:VkPhysicalDeviceMultiviewPropertiesKHX	 ;; :struct
   #:VkPhysicalDeviceProperties			 ;; :struct
   #:VkPhysicalDeviceProperties2KHR		 ;; :struct
   #:VkPhysicalDevicePushDescriptorPropertiesKHR ;; :struct
   #:VkPhysicalDeviceSparseImageFormatInfo2KHR	 ;; :struct
   #:VkPhysicalDeviceSparseProperties		 ;; :struct
   #:VkPhysicalDeviceSurfaceInfo2KHR		 ;; :struct
   #:VkPhysicalDeviceType			 ;; ENUM
   #:VkPipeline				       ;; NON-DISPATCH-HANDLE
   #:VkPipelineBindPoint		       ;; ENUM
   #:VkPipelineCache			       ;; NON-DISPATCH-HANDLE
   #:VkPipelineCacheCreateFlagBits	       ;; ENUM
   #:VkPipelineCacheCreateFlags		       ;; BITMASK
   #:VkPipelineCacheCreateInfo		       ;; :struct
   #:VkPipelineCacheHeaderVersion	       ;; ENUM
   #:VkPipelineColorBlendAttachmentState       ;; :struct
   #:VkPipelineColorBlendStateCreateFlagBits   ;; ENUM
   #:VkPipelineColorBlendStateCreateFlags      ;; BITMASK
   #:VkPipelineColorBlendStateCreateInfo       ;; :struct
   #:VkPipelineCreateFlagBits		       ;; ENUM
   #:VkPipelineCreateFlags		       ;; BITMASK
   #:VkPipelineDepthStencilStateCreateFlagBits ;; ENUM
   #:VkPipelineDepthStencilStateCreateFlags    ;; BITMASK
   #:VkPipelineDepthStencilStateCreateInfo     ;; :struct
   #:VkPipelineDiscardRectangleStateCreateFlagsEXT ;; BITMASK
   #:VkPipelineDiscardRectangleStateCreateInfoEXT  ;; :struct
   #:VkPipelineDynamicStateCreateFlagBits	   ;; ENUM
   #:VkPipelineDynamicStateCreateFlags		   ;; BITMASK
   #:VkPipelineDynamicStateCreateInfo		   ;; :struct
   #:VkPipelineInputAssemblyStateCreateFlagBits	   ;; ENUM
   #:VkPipelineInputAssemblyStateCreateFlags	   ;; BITMASK
   #:VkPipelineInputAssemblyStateCreateInfo	   ;; :struct
   #:VkPipelineLayout				;; NON-DISPATCH-HANDLE
   #:VkPipelineLayoutCreateFlagBits		;; ENUM
   #:VkPipelineLayoutCreateFlags		;; BITMASK
   #:VkPipelineLayoutCreateInfo			;; :struct
   #:VkPipelineMultisampleStateCreateFlagBits	;; ENUM
   #:VkPipelineMultisampleStateCreateFlags	;; BITMASK
   #:VkPipelineMultisampleStateCreateInfo	;; :struct
   #:VkPipelineRasterizationStateCreateFlagBits	;; ENUM
   #:VkPipelineRasterizationStateCreateFlags	;; BITMASK
   #:VkPipelineRasterizationStateCreateInfo	;; :struct
   #:VkPipelineRasterizationStateRasterizationOrderAMD ;; :struct
   #:VkPipelineShaderStageCreateFlagBits	       ;; ENUM
   #:VkPipelineShaderStageCreateFlags		       ;; BITMASK
   #:VkPipelineShaderStageCreateInfo		       ;; :struct
   #:VkPipelineStageFlagBits			       ;; ENUM
   #:VkPipelineStageFlags			       ;; BITMASK
   #:VkPipelineTessellationStateCreateFlagBits	       ;; ENUM
   #:VkPipelineTessellationStateCreateFlags	       ;; BITMASK
   #:VkPipelineTessellationStateCreateInfo	       ;; :struct
   #:VkPipelineVertexInputStateCreateFlagBits	       ;; ENUM
   #:VkPipelineVertexInputStateCreateFlags	       ;; BITMASK
   #:VkPipelineVertexInputStateCreateInfo	       ;; :struct
   #:VkPipelineViewportStateCreateFlagBits	       ;; ENUM
   #:VkPipelineViewportStateCreateFlags		       ;; BITMASK
   #:VkPipelineViewportStateCreateInfo		       ;; :struct
   #:VkPipelineViewportSwizzleStateCreateFlagsNV       ;; BITMASK
   #:VkPipelineViewportSwizzleStateCreateInfoNV	       ;; :struct
   #:VkPipelineViewportWScalingStateCreateInfoNV       ;; :struct
   #:VkPolygonMode				       ;; ENUM
   #:VkPresentInfoKHR				       ;; :struct
   #:VkPresentModeKHR				       ;; ENUM
   #:VkPresentRegionKHR				       ;; :struct
   #:VkPresentRegionsKHR			       ;; :struct
   #:VkPresentTimeGOOGLE			       ;; :struct
   #:VkPresentTimesInfoGOOGLE			       ;; :struct
   #:VkPrimitiveTopology			       ;; ENUM
   #:VkPushConstantRange			       ;; :struct
   #:VkQueryControlFlagBits			       ;; ENUM
   #:VkQueryControlFlags			       ;; BITMASK
   #:VkQueryPipelineStatisticFlagBits		       ;; ENUM
   #:VkQueryPipelineStatisticFlags		       ;; BITMASK
   #:VkQueryPool			    ;; NON-DISPATCH-HANDLE
   #:VkQueryPoolCreateFlagBits		    ;; ENUM
   #:VkQueryPoolCreateFlags		    ;; BITMASK
   #:VkQueryPoolCreateInfo		    ;; :struct
   #:VkQueryResultFlagBits		    ;; ENUM
   #:VkQueryResultFlags			    ;; BITMASK
   #:VkQueryType			    ;; ENUM
   #:VkQueue				    ;; HANDLE
   #:VkQueueFamilyProperties		    ;; :struct
   #:VkQueueFamilyProperties2KHR	    ;; :struct
   #:VkQueueFlagBits			    ;; ENUM
   #:VkQueueFlags			    ;; BITMASK
   #:VkRasterizationOrderAMD		    ;; ENUM
   #:VkRect2D				    ;; :struct
   #:VkRect3D				    ;; :struct
   #:VkRectLayerKHR			    ;; :struct
   #:VkRefreshCycleDurationGOOGLE	    ;; :struct
   #:VkRenderPass			    ;; NON-DISPATCH-HANDLE
   #:VkRenderPassBeginInfo		    ;; :struct
   #:VkRenderPassCreateFlagBits		    ;; ENUM
   #:VkRenderPassCreateFlags		    ;; BITMASK
   #:VkRenderPassCreateInfo		    ;; :struct
   #:VkRenderPassMultiviewCreateInfoKHX	    ;; :struct
   #:VkResult				    ;; ENUM
   #:VkSampleCountFlagBits		    ;; ENUM
   #:VkSampleCountFlags			    ;; BITMASK
   #:VkSampleMask			    ;; ALIAS
   #:VkSampler				    ;; NON-DISPATCH-HANDLE
   #:VkSamplerAddressMode		    ;; ENUM
   #:VkSamplerCreateFlagBits		    ;; ENUM
   #:VkSamplerCreateFlags		    ;; BITMASK
   #:VkSamplerCreateInfo		    ;; :struct
   #:VkSamplerMipmapMode		    ;; ENUM
   #:VkSemaphore			    ;; NON-DISPATCH-HANDLE
   #:VkSemaphoreCreateFlags		    ;; BITMASK
   #:VkSemaphoreCreateInfo		    ;; :struct
   #:VkShaderModule			    ;; NON-DISPATCH-HANDLE
   #:VkShaderModuleCreateFlags		    ;; BITMASK
   #:VkShaderModuleCreateInfo		    ;; :struct
   #:VkShaderStageFlagBits		    ;; ENUM
   #:VkShaderStageFlags			    ;; BITMASK
   #:VkSharedPresentSurfaceCapabilitiesKHR  ;; :struct
   #:VkSharingMode			    ;; ENUM
   #:VkSparseBufferMemoryBindInfo	    ;; :struct
   #:VkSparseImageFormatFlagBits	    ;; ENUM
   #:VkSparseImageFormatFlags		    ;; BITMASK
   #:VkSparseImageFormatProperties	    ;; :struct
   #:VkSparseImageFormatProperties2KHR	    ;; :struct
   #:VkSparseImageMemoryBind		    ;; :struct
   #:VkSparseImageMemoryBindInfo	    ;; :struct
   #:VkSparseImageMemoryRequirements	    ;; :struct
   #:VkSparseImageOpaqueMemoryBindInfo	    ;; :struct
   #:VkSparseMemoryBind			    ;; :struct
   #:VkSparseMemoryBindFlagBits		    ;; ENUM
   #:VkSparseMemoryBindFlags		    ;; BITMASK
   #:VkSpecializationInfo		    ;; :struct
   #:VkSpecializationMapEntry		    ;; :struct
   #:VkStencilFaceFlagBits		    ;; ENUM
   #:VkStencilFaceFlags			    ;; BITMASK
   #:VkStencilOp			    ;; ENUM
   #:VkStencilOpState			    ;; :struct
   #:VkStructureType			    ;; ENUM
   #:VkSubmitInfo			    ;; :struct
   #:VkSubpassContents			    ;; ENUM
   #:VkSubpassDependency		    ;; :struct
   #:VkSubpassDescription		    ;; :struct
   #:VkSubpassDescriptionFlagBits	    ;; ENUM
   #:VkSubpassDescriptionFlags		    ;; BITMASK
   #:VkSubresourceLayout		    ;; :struct
   #:VkSurfaceCapabilities2EXT		    ;; :struct
   #:VkSurfaceCapabilities2KHR		    ;; :struct
   #:VkSurfaceCapabilitiesKHR		    ;; :struct
   #:VkSurfaceCounterFlagBitsEXT	    ;; ENUM
   #:VkSurfaceCounterFlagsEXT		    ;; BITMASK
   #:VkSurfaceFormat2KHR		    ;; :struct
   #:VkSurfaceFormatKHR			    ;; :struct
   #:VkSurfaceKHR			    ;; NON-DISPATCH-HANDLE
   #:VkSurfaceTransformFlagBitsKHR	    ;; ENUM
   #:VkSurfaceTransformFlagsKHR		    ;; BITMASK
   #:VkSwapchainCounterCreateInfoEXT	    ;; :struct
   #:VkSwapchainCreateFlagBitsKHR	    ;; ENUM
   #:VkSwapchainCreateFlagsKHR		    ;; BITMASK
   #:VkSwapchainCreateInfoKHR		    ;; :struct
   #:VkSwapchainKHR			    ;; NON-DISPATCH-HANDLE
   #:VkSystemAllocationScope		    ;; ENUM
   #:VkTextureLODGatherFormatPropertiesAMD  ;; :struct
   #:VkValidationCheckEXT		    ;; ENUM
   #:VkValidationFlagsEXT		    ;; :struct
   #:VkVertexInputAttributeDescription	    ;; :struct
   #:VkVertexInputBindingDescription	    ;; :struct
   #:VkVertexInputRate			    ;; ENUM
   #:VkViSurfaceCreateFlagsNN		    ;; BITMASK
   #:VkViSurfaceCreateInfoNN		    ;; :struct
   #:VkViewport				    ;; :struct
   #:VkViewportCoordinateSwizzleNV	    ;; ENUM
   #:VkViewportSwizzleNV		    ;; :struct
   #:VkViewportWScalingNV		    ;; :struct
   #:VkWaylandSurfaceCreateFlagsKHR	    ;; BITMASK
   #:VkWaylandSurfaceCreateInfoKHR	    ;; :struct
   #:VkWin32KeyedMutexAcquireReleaseInfoKHX ;; :struct
   #:VkWin32KeyedMutexAcquireReleaseInfoNV  ;; :struct
   #:VkWin32SurfaceCreateFlagsKHR	    ;; BITMASK
   #:VkWin32SurfaceCreateInfoKHR	    ;; :struct
   #:VkWriteDescriptorSet		    ;; :struct
   #:VkXYColorEXT			    ;; :struct
   #:VkXcbSurfaceCreateFlagsKHR		    ;; BITMASK
   #:VkXcbSurfaceCreateInfoKHR		    ;; :struct
   #:VkXlibSurfaceCreateFlagsKHR	    ;; BITMASK
   #:VkXlibSurfaceCreateInfoKHR		    ;; :struct

   #:vkAcquireNextImage2KHX
   #:vkAcquireNextImageKHR
   #:vkAcquireXlibDisplayEXT
   #:vkAllocateCommandBuffers
   #:vkAllocateDescriptorSets
   #:vkAllocateMemory
   #:vkBeginCommandBuffer
   #:vkBindBufferMemory
   #:vkBindBufferMemory2KHX
   #:vkBindImageMemory
   #:vkBindImageMemory2KHX
   #:vkCmdBeginQuery
   #:vkCmdBeginRenderPass
   #:vkCmdBindDescriptorSets
   #:vkCmdBindIndexBuffer
   #:vkCmdBindPipeline
   #:vkCmdBindVertexBuffers
   #:vkCmdBlitImage
   #:vkCmdClearAttachments
   #:vkCmdClearColorImage
   #:vkCmdClearDepthStencilImage
   #:vkCmdCopyBuffer
   #:vkCmdCopyBufferToImage
   #:vkCmdCopyImage
   #:vkCmdCopyImageToBuffer
   #:vkCmdCopyQueryPoolResults
   #:vkCmdDebugMarkerBeginEXT
   #:vkCmdDebugMarkerEndEXT
   #:vkCmdDebugMarkerInsertEXT
   #:vkCmdDispatch
   #:vkCmdDispatchBaseKHX
   #:vkCmdDispatchIndirect
   #:vkCmdDraw
   #:vkCmdDrawIndexed
   #:vkCmdDrawIndexedIndirect
   #:vkCmdDrawIndexedIndirectCountAMD
   #:vkCmdDrawIndirect
   #:vkCmdDrawIndirectCountAMD
   #:vkCmdEndQuery
   #:vkCmdEndRenderPass
   #:vkCmdExecuteCommands
   #:vkCmdFillBuffer
   #:vkCmdNextSubpass
   #:vkCmdPipelineBarrier
   #:vkCmdProcessCommandsNVX
   #:vkCmdPushConstants
   #:vkCmdPushDescriptorSetKHR
   #:vkCmdPushDescriptorSetWithTemplateKHR
   #:vkCmdReserveSpaceForCommandsNVX
   #:vkCmdResetEvent
   #:vkCmdResetQueryPool
   #:vkCmdResolveImage
   #:vkCmdSetBlendConstants
   #:vkCmdSetDepthBias
   #:vkCmdSetDepthBounds
   #:vkCmdSetDeviceMaskKHX
   #:vkCmdSetDiscardRectangleEXT
   #:vkCmdSetEvent
   #:vkCmdSetLineWidth
   #:vkCmdSetScissor
   #:vkCmdSetStencilCompareMask
   #:vkCmdSetStencilReference
   #:vkCmdSetStencilWriteMask
   #:vkCmdSetViewport
   #:vkCmdSetViewportWScalingNV
   #:vkCmdUpdateBuffer
   #:vkCmdWaitEvents
   #:vkCmdWriteTimestamp
   #:vkCreateAndroidSurfaceKHR
   #:vkCreateBuffer
   #:vkCreateBufferView
   #:vkCreateCommandPool
   #:vkCreateComputePipelines
   #:vkCreateDebugReportCallbackEXT
   #:vkCreateDescriptorPool
   #:vkCreateDescriptorSetLayout
   #:vkCreateDescriptorUpdateTemplateKHR
   #:vkCreateDevice
   #:vkCreateDisplayModeKHR
   #:vkCreateDisplayPlaneSurfaceKHR
   #:vkCreateEvent
   #:vkCreateFence
   #:vkCreateFramebuffer
   #:vkCreateGraphicsPipelines
   #:vkCreateIOSSurfaceMVK
   #:vkCreateImage
   #:vkCreateImageView
   #:vkCreateIndirectCommandsLayoutNVX
   #:vkCreateInstance
   #:vkCreateMacOSSurfaceMVK
   #:vkCreateMirSurfaceKHR
   #:vkCreateObjectTableNVX
   #:vkCreatePipelineCache
   #:vkCreatePipelineLayout
   #:vkCreateQueryPool
   #:vkCreateRenderPass
   #:vkCreateSampler
   #:vkCreateSemaphore
   #:vkCreateShaderModule
   #:vkCreateSharedSwapchainsKHR
   #:vkCreateSwapchainKHR
   #:vkCreateViSurfaceNN
   #:vkCreateWaylandSurfaceKHR
   #:vkCreateWin32SurfaceKHR
   #:vkCreateXcbSurfaceKHR
   #:vkCreateXlibSurfaceKHR
   #:vkDebugMarkerSetObjectNameEXT
   #:vkDebugMarkerSetObjectTagEXT
   #:vkDebugReportMessageEXT
   #:vkDestroyBuffer
   #:vkDestroyBufferView
   #:vkDestroyCommandPool
   #:vkDestroyDebugReportCallbackEXT
   #:vkDestroyDescriptorPool
   #:vkDestroyDescriptorSetLayout
   #:vkDestroyDescriptorUpdateTemplateKHR
   #:vkDestroyDevice
   #:vkDestroyEvent
   #:vkDestroyFence
   #:vkDestroyFramebuffer
   #:vkDestroyImage
   #:vkDestroyImageView
   #:vkDestroyIndirectCommandsLayoutNVX
   #:vkDestroyInstance
   #:vkDestroyObjectTableNVX
   #:vkDestroyPipeline
   #:vkDestroyPipelineCache
   #:vkDestroyPipelineLayout
   #:vkDestroyQueryPool
   #:vkDestroyRenderPass
   #:vkDestroySampler
   #:vkDestroySemaphore
   #:vkDestroyShaderModule
   #:vkDestroySurfaceKHR
   #:vkDestroySwapchainKHR
   #:vkDeviceWaitIdle
   #:vkDisplayPowerControlEXT
   #:vkEndCommandBuffer
   #:vkEnumerateDeviceExtensionProperties
   #:vkEnumerateDeviceLayerProperties
   #:vkEnumerateInstanceExtensionProperties
   #:vkEnumerateInstanceLayerProperties
   #:vkEnumeratePhysicalDeviceGroupsKHX
   #:vkEnumeratePhysicalDevices
   #:vkFlushMappedMemoryRanges
   #:vkFreeCommandBuffers
   #:vkFreeDescriptorSets
   #:vkFreeMemory
   #:vkGetBufferMemoryRequirements
   #:vkGetDeviceGroupPeerMemoryFeaturesKHX
   #:vkGetDeviceGroupPresentCapabilitiesKHX
   #:vkGetDeviceGroupSurfacePresentModesKHX
   #:vkGetDeviceMemoryCommitment
   #:vkGetDeviceProcAddr
   #:vkGetDeviceQueue
   #:vkGetDisplayModePropertiesKHR
   #:vkGetDisplayPlaneCapabilitiesKHR
   #:vkGetDisplayPlaneSupportedDisplaysKHR
   #:vkGetEventStatus
   #:vkGetFenceStatus
   #:vkGetImageMemoryRequirements
   #:vkGetImageSparseMemoryRequirements
   #:vkGetImageSubresourceLayout
   #:vkGetInstanceProcAddr
   #:vkGetMemoryFdKHX
   #:vkGetMemoryFdPropertiesKHX
   #:vkGetMemoryWin32HandleKHX
   #:vkGetMemoryWin32HandleNV
   #:vkGetMemoryWin32HandlePropertiesKHX
   #:vkGetPastPresentationTimingGOOGLE
   #:vkGetPhysicalDeviceDisplayPlanePropertiesKHR
   #:vkGetPhysicalDeviceDisplayPropertiesKHR
   #:vkGetPhysicalDeviceExternalBufferPropertiesKHX
   #:vkGetPhysicalDeviceExternalImageFormatPropertiesNV
   #:vkGetPhysicalDeviceExternalSemaphorePropertiesKHX
   #:vkGetPhysicalDeviceFeatures
   #:vkGetPhysicalDeviceFeatures2KHR
   #:vkGetPhysicalDeviceFormatProperties
   #:vkGetPhysicalDeviceFormatProperties2KHR
   #:vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
   #:vkGetPhysicalDeviceImageFormatProperties
   #:vkGetPhysicalDeviceImageFormatProperties2KHR
   #:vkGetPhysicalDeviceMemoryProperties
   #:vkGetPhysicalDeviceMemoryProperties2KHR
   #:vkGetPhysicalDeviceMirPresentationSupportKHR
   #:vkGetPhysicalDevicePresentRectanglesKHX
   #:vkGetPhysicalDeviceProperties
   #:vkGetPhysicalDeviceProperties2KHR
   #:vkGetPhysicalDeviceQueueFamilyProperties
   #:vkGetPhysicalDeviceQueueFamilyProperties2KHR
   #:vkGetPhysicalDeviceSparseImageFormatProperties
   #:vkGetPhysicalDeviceSparseImageFormatProperties2KHR
   #:vkGetPhysicalDeviceSurfaceCapabilities2EXT
   #:vkGetPhysicalDeviceSurfaceCapabilities2KHR
   #:vkGetPhysicalDeviceSurfaceCapabilitiesKHR
   #:vkGetPhysicalDeviceSurfaceFormats2KHR
   #:vkGetPhysicalDeviceSurfaceFormatsKHR
   #:vkGetPhysicalDeviceSurfacePresentModesKHR
   #:vkGetPhysicalDeviceSurfaceSupportKHR
   #:vkGetPhysicalDeviceWaylandPresentationSupportKHR
   #:vkGetPhysicalDeviceWin32PresentationSupportKHR
   #:vkGetPhysicalDeviceXcbPresentationSupportKHR
   #:vkGetPhysicalDeviceXlibPresentationSupportKHR
   #:vkGetPipelineCacheData
   #:vkGetQueryPoolResults
   #:vkGetRandROutputDisplayEXT
   #:vkGetRefreshCycleDurationGOOGLE
   #:vkGetRenderAreaGranularity
   #:vkGetSemaphoreFdKHX
   #:vkGetSemaphoreWin32HandleKHX
   #:vkGetSwapchainCounterEXT
   #:vkGetSwapchainImagesKHR
   #:vkGetSwapchainStatusKHR
   #:vkImportSemaphoreFdKHX
   #:vkImportSemaphoreWin32HandleKHX
   #:vkInvalidateMappedMemoryRanges
   #:vkMapMemory
   #:vkMergePipelineCaches
   #:vkQueueBindSparse
   #:vkQueuePresentKHR
   #:vkQueueSubmit
   #:vkQueueWaitIdle
   #:vkRegisterDeviceEventEXT
   #:vkRegisterDisplayEventEXT
   #:vkRegisterObjectsNVX
   #:vkReleaseDisplayEXT
   #:vkResetCommandBuffer
   #:vkResetCommandPool
   #:vkResetDescriptorPool
   #:vkResetEvent
   #:vkResetFences
   #:vkSetEvent
   #:vkSetHdrMetadataEXT
   #:vkTrimCommandPoolKHR
   #:vkUnmapMemory
   #:vkUnregisterObjectsNVX
   #:vkUpdateDescriptorSetWithTemplateKHR
   #:vkUpdateDescriptorSets
   #:vkWaitForFences
   #:VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV
   #:VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV
   #:VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV
   #:VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV
   #:VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV
   #:VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV
   #:VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV
   #:VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV
   #:VK_VERTEX_INPUT_RATE_INSTANCE
   #:VK_VERTEX_INPUT_RATE_VERTEX
   #:VK_VALIDATION_CHECK_SHADERS_EXT
   #:VK_VALIDATION_CHECK_ALL_EXT
   #:VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE
   #:VK_SYSTEM_ALLOCATION_SCOPE_DEVICE
   #:VK_SYSTEM_ALLOCATION_SCOPE_CACHE
   #:VK_SYSTEM_ALLOCATION_SCOPE_OBJECT
   #:VK_SYSTEM_ALLOCATION_SCOPE_COMMAND
   #:VK_SWAPCHAIN_CREATE_BIND_SFR_BIT_KHX
   #:VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR
   #:VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR
   #:VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR
   #:VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR
   #:VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR
   #:VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR
   #:VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR
   #:VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR
   #:VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
   #:VK_SURFACE_COUNTER_VBLANK_EXT
   #:VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX
   #:VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX
   #:VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
   #:VK_SUBPASS_CONTENTS_INLINE
   #:VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK
   #:VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK
   #:VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR
   #:VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR
   #:VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR
   #:VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR
   #:VK_STRUCTURE_TYPE_HDR_METADATA_EXT
   #:VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT
   #:VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT
   #:VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV
   #:VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX
   #:VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE
   #:VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT
   #:VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT
   #:VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT
   #:VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT
   #:VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT
   #:VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
   #:VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX
   #:VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX
   #:VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX
   #:VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX
   #:VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX
   #:VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX
   #:VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR
   #:VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR
   #:VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
   #:VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHX
   #:VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHX
   #:VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHX
   #:VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHX
   #:VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHX
   #:VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHX
   #:VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHX
   #:VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHX
   #:VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHX
   #:VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHX
   #:VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHX
   #:VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHX
   #:VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHX
   #:VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHX
   #:VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHX
   #:VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHX
   #:VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHX
   #:VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHX
   #:VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHX
   #:VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHX
   #:VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHX
   #:VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHX
   #:VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHX
   #:VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN
   #:VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT
   #:VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHX
   #:VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHX
   #:VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHX
   #:VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHX
   #:VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHX
   #:VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHX
   #:VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHX
   #:VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHX
   #:VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHX
   #:VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHX
   #:VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHX
   #:VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHX
   #:VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHX
   #:VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR
   #:VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR
   #:VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR
   #:VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR
   #:VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR
   #:VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR
   #:VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR
   #:VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR
   #:VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR
   #:VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
   #:VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV
   #:VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV
   #:VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV
   #:VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV
   #:VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHX
   #:VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHX
   #:VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHX
   #:VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD
   #:VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
   #:VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
   #:VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
   #:VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT
   #:VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT
   #:VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT
   #:VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD
   #:VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
   #:VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR
   #:VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR
   #:VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR
   #:VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR
   #:VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR
   #:VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR
   #:VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR
   #:VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR
   #:VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR
   #:VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
   #:VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
   #:VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_MEMORY_BARRIER
   #:VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
   #:VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER
   #:VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
   #:VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
   #:VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO
   #:VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
   #:VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
   #:VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
   #:VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
   #:VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET
   #:VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
   #:VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
   #:VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
   #:VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
   #:VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO
   #:VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
   #:VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
   #:VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO
   #:VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
   #:VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO
   #:VK_STRUCTURE_TYPE_EVENT_CREATE_INFO
   #:VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_BIND_SPARSE_INFO
   #:VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
   #:VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
   #:VK_STRUCTURE_TYPE_SUBMIT_INFO
   #:VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
   #:VK_STRUCTURE_TYPE_APPLICATION_INFO
   #:VK_STENCIL_OP_DECREMENT_AND_WRAP
   #:VK_STENCIL_OP_INCREMENT_AND_WRAP
   #:VK_STENCIL_OP_INVERT
   #:VK_STENCIL_OP_DECREMENT_AND_CLAMP
   #:VK_STENCIL_OP_INCREMENT_AND_CLAMP
   #:VK_STENCIL_OP_REPLACE
   #:VK_STENCIL_OP_ZERO
   #:VK_STENCIL_OP_KEEP
   #:VK_STENCIL_FRONT_AND_BACK
   #:VK_STENCIL_FACE_BACK_BIT
   #:VK_STENCIL_FACE_FRONT_BIT
   #:VK_SPARSE_MEMORY_BIND_METADATA_BIT
   #:VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT
   #:VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT
   #:VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT
   #:VK_SHARING_MODE_CONCURRENT
   #:VK_SHARING_MODE_EXCLUSIVE
   #:VK_SHADER_STAGE_ALL
   #:VK_SHADER_STAGE_ALL_GRAPHICS
   #:VK_SHADER_STAGE_COMPUTE_BIT
   #:VK_SHADER_STAGE_FRAGMENT_BIT
   #:VK_SHADER_STAGE_GEOMETRY_BIT
   #:VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT
   #:VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT
   #:VK_SHADER_STAGE_VERTEX_BIT
   #:VK_SAMPLER_MIPMAP_MODE_LINEAR
   #:VK_SAMPLER_MIPMAP_MODE_NEAREST
   #:VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE
   #:VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
   #:VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
   #:VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
   #:VK_SAMPLER_ADDRESS_MODE_REPEAT
   #:VK_SAMPLE_COUNT_64_BIT
   #:VK_SAMPLE_COUNT_32_BIT
   #:VK_SAMPLE_COUNT_16_BIT
   #:VK_SAMPLE_COUNT_8_BIT
   #:VK_SAMPLE_COUNT_4_BIT
   #:VK_SAMPLE_COUNT_2_BIT
   #:VK_SAMPLE_COUNT_1_BIT
   #:VK_ERROR_INVALID_EXTERNAL_HANDLE_KHX
   #:VK_ERROR_OUT_OF_POOL_MEMORY_KHR
   #:VK_NV_EXTENSION_1_ERROR
   #:VK_ERROR_INVALID_SHADER_NV
   #:VK_ERROR_VALIDATION_FAILED_EXT
   #:VK_ERROR_INCOMPATIBLE_DISPLAY_KHR
   #:VK_ERROR_OUT_OF_DATE_KHR
   #:VK_SUBOPTIMAL_KHR
   #:VK_ERROR_NATIVE_WINDOW_IN_USE_KHR
   #:VK_ERROR_SURFACE_LOST_KHR
   #:VK_ERROR_FRAGMENTED_POOL
   #:VK_ERROR_FORMAT_NOT_SUPPORTED
   #:VK_ERROR_TOO_MANY_OBJECTS
   #:VK_ERROR_INCOMPATIBLE_DRIVER
   #:VK_ERROR_FEATURE_NOT_PRESENT
   #:VK_ERROR_EXTENSION_NOT_PRESENT
   #:VK_ERROR_LAYER_NOT_PRESENT
   #:VK_ERROR_MEMORY_MAP_FAILED
   #:VK_ERROR_DEVICE_LOST
   #:VK_ERROR_INITIALIZATION_FAILED
   #:VK_ERROR_OUT_OF_DEVICE_MEMORY
   #:VK_ERROR_OUT_OF_HOST_MEMORY
   #:VK_INCOMPLETE
   #:VK_EVENT_RESET
   #:VK_EVENT_SET
   #:VK_TIMEOUT
   #:VK_NOT_READY
   #:VK_SUCCESS
   #:VK_RASTERIZATION_ORDER_RELAXED_AMD
   #:VK_RASTERIZATION_ORDER_STRICT_AMD
   #:VK_QUEUE_SPARSE_BINDING_BIT
   #:VK_QUEUE_TRANSFER_BIT
   #:VK_QUEUE_COMPUTE_BIT
   #:VK_QUEUE_GRAPHICS_BIT
   #:VK_QUERY_TYPE_TIMESTAMP
   #:VK_QUERY_TYPE_PIPELINE_STATISTICS
   #:VK_QUERY_TYPE_OCCLUSION
   #:VK_QUERY_RESULT_PARTIAL_BIT
   #:VK_QUERY_RESULT_WITH_AVAILABILITY_BIT
   #:VK_QUERY_RESULT_WAIT_BIT
   #:VK_QUERY_RESULT_64_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT
   #:VK_QUERY_CONTROL_PRECISE_BIT
   #:VK_PRIMITIVE_TOPOLOGY_PATCH_LIST
   #:VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY
   #:VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY
   #:VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY
   #:VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY
   #:VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN
   #:VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP
   #:VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
   #:VK_PRIMITIVE_TOPOLOGY_LINE_STRIP
   #:VK_PRIMITIVE_TOPOLOGY_LINE_LIST
   #:VK_PRIMITIVE_TOPOLOGY_POINT_LIST
   #:VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR
   #:VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR
   #:VK_PRESENT_MODE_FIFO_RELAXED_KHR
   #:VK_PRESENT_MODE_FIFO_KHR
   #:VK_PRESENT_MODE_MAILBOX_KHR
   #:VK_PRESENT_MODE_IMMEDIATE_KHR
   #:VK_POLYGON_MODE_POINT
   #:VK_POLYGON_MODE_LINE
   #:VK_POLYGON_MODE_FILL
   #:VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX
   #:VK_PIPELINE_STAGE_ALL_COMMANDS_BIT
   #:VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT
   #:VK_PIPELINE_STAGE_HOST_BIT
   #:VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
   #:VK_PIPELINE_STAGE_TRANSFER_BIT
   #:VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT
   #:VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
   #:VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
   #:VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
   #:VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
   #:VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
   #:VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
   #:VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT
   #:VK_PIPELINE_STAGE_VERTEX_SHADER_BIT
   #:VK_PIPELINE_STAGE_VERTEX_INPUT_BIT
   #:VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT
   #:VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
   #:VK_PIPELINE_CREATE_DISPATCH_BASE_KHX
   #:VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHX
   #:VK_PIPELINE_CREATE_DERIVATIVE_BIT
   #:VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT
   #:VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT
   #:VK_PIPELINE_CACHE_HEADER_VERSION_ONE
   #:VK_PIPELINE_BIND_POINT_COMPUTE
   #:VK_PIPELINE_BIND_POINT_GRAPHICS
   #:VK_PHYSICAL_DEVICE_TYPE_CPU
   #:VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU
   #:VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
   #:VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
   #:VK_PHYSICAL_DEVICE_TYPE_OTHER
   #:VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX
   #:VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX
   #:VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX
   #:VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX
   #:VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX
   #:VK_OBJECT_TYPE_OBJECT_TABLE_NVX
   #:VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR
   #:VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT
   #:VK_OBJECT_TYPE_DISPLAY_MODE_KHR
   #:VK_OBJECT_TYPE_DISPLAY_KHR
   #:VK_OBJECT_TYPE_SWAPCHAIN_KHR
   #:VK_OBJECT_TYPE_SURFACE_KHR
   #:VK_OBJECT_TYPE_COMMAND_POOL
   #:VK_OBJECT_TYPE_FRAMEBUFFER
   #:VK_OBJECT_TYPE_DESCRIPTOR_SET
   #:VK_OBJECT_TYPE_DESCRIPTOR_POOL
   #:VK_OBJECT_TYPE_SAMPLER
   #:VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT
   #:VK_OBJECT_TYPE_PIPELINE
   #:VK_OBJECT_TYPE_RENDER_PASS
   #:VK_OBJECT_TYPE_PIPELINE_LAYOUT
   #:VK_OBJECT_TYPE_PIPELINE_CACHE
   #:VK_OBJECT_TYPE_SHADER_MODULE
   #:VK_OBJECT_TYPE_IMAGE_VIEW
   #:VK_OBJECT_TYPE_BUFFER_VIEW
   #:VK_OBJECT_TYPE_QUERY_POOL
   #:VK_OBJECT_TYPE_EVENT
   #:VK_OBJECT_TYPE_IMAGE
   #:VK_OBJECT_TYPE_BUFFER
   #:VK_OBJECT_TYPE_DEVICE_MEMORY
   #:VK_OBJECT_TYPE_FENCE
   #:VK_OBJECT_TYPE_COMMAND_BUFFER
   #:VK_OBJECT_TYPE_SEMAPHORE
   #:VK_OBJECT_TYPE_QUEUE
   #:VK_OBJECT_TYPE_DEVICE
   #:VK_OBJECT_TYPE_PHYSICAL_DEVICE
   #:VK_OBJECT_TYPE_INSTANCE
   #:VK_OBJECT_TYPE_UNKNOWN
   #:VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX
   #:VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX
   #:VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX
   #:VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX
   #:VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX
   #:VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX
   #:VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX
   #:VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT
   #:VK_MEMORY_PROPERTY_HOST_CACHED_BIT
   #:VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
   #:VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
   #:VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
   #:VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHX
   #:VK_MEMORY_HEAP_DEVICE_LOCAL_BIT
   #:VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX
   #:VK_LOGIC_OP_SET
   #:VK_LOGIC_OP_NAND
   #:VK_LOGIC_OP_OR_INVERTED
   #:VK_LOGIC_OP_COPY_INVERTED
   #:VK_LOGIC_OP_OR_REVERSE
   #:VK_LOGIC_OP_INVERT
   #:VK_LOGIC_OP_EQUIVALENT
   #:VK_LOGIC_OP_NOR
   #:VK_LOGIC_OP_OR
   #:VK_LOGIC_OP_XOR
   #:VK_LOGIC_OP_NO_OP
   #:VK_LOGIC_OP_AND_INVERTED
   #:VK_LOGIC_OP_COPY
   #:VK_LOGIC_OP_AND_REVERSE
   #:VK_LOGIC_OP_AND
   #:VK_LOGIC_OP_CLEAR
   #:VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE
   #:VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX
   #:VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX
   #:VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX
   #:VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX
   #:VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX
   #:VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX
   #:VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX
   #:VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX
   #:VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX
   #:VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX
   #:VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX
   #:VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX
   #:VK_INDEX_TYPE_UINT32
   #:VK_INDEX_TYPE_UINT16
   #:VK_IMAGE_VIEW_TYPE_CUBE_ARRAY
   #:VK_IMAGE_VIEW_TYPE_2D_ARRAY
   #:VK_IMAGE_VIEW_TYPE_1D_ARRAY
   #:VK_IMAGE_VIEW_TYPE_CUBE
   #:VK_IMAGE_VIEW_TYPE_3D
   #:VK_IMAGE_VIEW_TYPE_2D
   #:VK_IMAGE_VIEW_TYPE_1D
   #:VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT
   #:VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
   #:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
   #:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
   #:VK_IMAGE_USAGE_STORAGE_BIT
   #:VK_IMAGE_USAGE_SAMPLED_BIT
   #:VK_IMAGE_USAGE_TRANSFER_DST_BIT
   #:VK_IMAGE_USAGE_TRANSFER_SRC_BIT
   #:VK_IMAGE_TYPE_3D
   #:VK_IMAGE_TYPE_2D
   #:VK_IMAGE_TYPE_1D
   #:VK_IMAGE_TILING_LINEAR
   #:VK_IMAGE_TILING_OPTIMAL
   #:VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR
   #:VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
   #:VK_IMAGE_LAYOUT_PREINITIALIZED
   #:VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
   #:VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
   #:VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
   #:VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
   #:VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
   #:VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
   #:VK_IMAGE_LAYOUT_GENERAL
   #:VK_IMAGE_LAYOUT_UNDEFINED
   #:VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR
   #:VK_IMAGE_CREATE_BIND_SFR_BIT_KHX
   #:VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT
   #:VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT
   #:VK_IMAGE_CREATE_SPARSE_ALIASED_BIT
   #:VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
   #:VK_IMAGE_CREATE_SPARSE_BINDING_BIT
   #:VK_IMAGE_ASPECT_METADATA_BIT
   #:VK_IMAGE_ASPECT_STENCIL_BIT
   #:VK_IMAGE_ASPECT_DEPTH_BIT
   #:VK_IMAGE_ASPECT_COLOR_BIT
   #:VK_FRONT_FACE_CLOCKWISE
   #:VK_FRONT_FACE_COUNTER_CLOCKWISE
   #:VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR
   #:VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR
   #:VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG
   #:VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT
   #:VK_FORMAT_FEATURE_BLIT_DST_BIT
   #:VK_FORMAT_FEATURE_BLIT_SRC_BIT
   #:VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT
   #:VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT
   #:VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT
   #:VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT
   #:VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT
   #:VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT
   #:VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT
   #:VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT
   #:VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT
   #:VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT
   #:VK_FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG
   #:VK_FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG
   #:VK_FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG
   #:VK_FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG
   #:VK_FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG
   #:VK_FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG
   #:VK_FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG
   #:VK_FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG
   #:VK_FORMAT_ASTC_12x12_SRGB_BLOCK
   #:VK_FORMAT_ASTC_12x12_UNORM_BLOCK
   #:VK_FORMAT_ASTC_12x10_SRGB_BLOCK
   #:VK_FORMAT_ASTC_12x10_UNORM_BLOCK
   #:VK_FORMAT_ASTC_10x10_SRGB_BLOCK
   #:VK_FORMAT_ASTC_10x10_UNORM_BLOCK
   #:VK_FORMAT_ASTC_10x8_SRGB_BLOCK
   #:VK_FORMAT_ASTC_10x8_UNORM_BLOCK
   #:VK_FORMAT_ASTC_10x6_SRGB_BLOCK
   #:VK_FORMAT_ASTC_10x6_UNORM_BLOCK
   #:VK_FORMAT_ASTC_10x5_SRGB_BLOCK
   #:VK_FORMAT_ASTC_10x5_UNORM_BLOCK
   #:VK_FORMAT_ASTC_8x8_SRGB_BLOCK
   #:VK_FORMAT_ASTC_8x8_UNORM_BLOCK
   #:VK_FORMAT_ASTC_8x6_SRGB_BLOCK
   #:VK_FORMAT_ASTC_8x6_UNORM_BLOCK
   #:VK_FORMAT_ASTC_8x5_SRGB_BLOCK
   #:VK_FORMAT_ASTC_8x5_UNORM_BLOCK
   #:VK_FORMAT_ASTC_6x6_SRGB_BLOCK
   #:VK_FORMAT_ASTC_6x6_UNORM_BLOCK
   #:VK_FORMAT_ASTC_6x5_SRGB_BLOCK
   #:VK_FORMAT_ASTC_6x5_UNORM_BLOCK
   #:VK_FORMAT_ASTC_5x5_SRGB_BLOCK
   #:VK_FORMAT_ASTC_5x5_UNORM_BLOCK
   #:VK_FORMAT_ASTC_5x4_SRGB_BLOCK
   #:VK_FORMAT_ASTC_5x4_UNORM_BLOCK
   #:VK_FORMAT_ASTC_4x4_SRGB_BLOCK
   #:VK_FORMAT_ASTC_4x4_UNORM_BLOCK
   #:VK_FORMAT_EAC_R11G11_SNORM_BLOCK
   #:VK_FORMAT_EAC_R11G11_UNORM_BLOCK
   #:VK_FORMAT_EAC_R11_SNORM_BLOCK
   #:VK_FORMAT_EAC_R11_UNORM_BLOCK
   #:VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK
   #:VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK
   #:VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK
   #:VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK
   #:VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK
   #:VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK
   #:VK_FORMAT_BC7_SRGB_BLOCK
   #:VK_FORMAT_BC7_UNORM_BLOCK
   #:VK_FORMAT_BC6H_SFLOAT_BLOCK
   #:VK_FORMAT_BC6H_UFLOAT_BLOCK
   #:VK_FORMAT_BC5_SNORM_BLOCK
   #:VK_FORMAT_BC5_UNORM_BLOCK
   #:VK_FORMAT_BC4_SNORM_BLOCK
   #:VK_FORMAT_BC4_UNORM_BLOCK
   #:VK_FORMAT_BC3_SRGB_BLOCK
   #:VK_FORMAT_BC3_UNORM_BLOCK
   #:VK_FORMAT_BC2_SRGB_BLOCK
   #:VK_FORMAT_BC2_UNORM_BLOCK
   #:VK_FORMAT_BC1_RGBA_SRGB_BLOCK
   #:VK_FORMAT_BC1_RGBA_UNORM_BLOCK
   #:VK_FORMAT_BC1_RGB_SRGB_BLOCK
   #:VK_FORMAT_BC1_RGB_UNORM_BLOCK
   #:VK_FORMAT_D32_SFLOAT_S8_UINT
   #:VK_FORMAT_D24_UNORM_S8_UINT
   #:VK_FORMAT_D16_UNORM_S8_UINT
   #:VK_FORMAT_S8_UINT
   #:VK_FORMAT_D32_SFLOAT
   #:VK_FORMAT_X8_D24_UNORM_PACK32
   #:VK_FORMAT_D16_UNORM
   #:VK_FORMAT_E5B9G9R9_UFLOAT_PACK32
   #:VK_FORMAT_B10G11R11_UFLOAT_PACK32
   #:VK_FORMAT_R64G64B64A64_SFLOAT
   #:VK_FORMAT_R64G64B64A64_SINT
   #:VK_FORMAT_R64G64B64A64_UINT
   #:VK_FORMAT_R64G64B64_SFLOAT
   #:VK_FORMAT_R64G64B64_SINT
   #:VK_FORMAT_R64G64B64_UINT
   #:VK_FORMAT_R64G64_SFLOAT
   #:VK_FORMAT_R64G64_SINT
   #:VK_FORMAT_R64G64_UINT
   #:VK_FORMAT_R64_SFLOAT
   #:VK_FORMAT_R64_SINT
   #:VK_FORMAT_R64_UINT
   #:VK_FORMAT_R32G32B32A32_SFLOAT
   #:VK_FORMAT_R32G32B32A32_SINT
   #:VK_FORMAT_R32G32B32A32_UINT
   #:VK_FORMAT_R32G32B32_SFLOAT
   #:VK_FORMAT_R32G32B32_SINT
   #:VK_FORMAT_R32G32B32_UINT
   #:VK_FORMAT_R32G32_SFLOAT
   #:VK_FORMAT_R32G32_SINT
   #:VK_FORMAT_R32G32_UINT
   #:VK_FORMAT_R32_SFLOAT
   #:VK_FORMAT_R32_SINT
   #:VK_FORMAT_R32_UINT
   #:VK_FORMAT_R16G16B16A16_SFLOAT
   #:VK_FORMAT_R16G16B16A16_SINT
   #:VK_FORMAT_R16G16B16A16_UINT
   #:VK_FORMAT_R16G16B16A16_SSCALED
   #:VK_FORMAT_R16G16B16A16_USCALED
   #:VK_FORMAT_R16G16B16A16_SNORM
   #:VK_FORMAT_R16G16B16A16_UNORM
   #:VK_FORMAT_R16G16B16_SFLOAT
   #:VK_FORMAT_R16G16B16_SINT
   #:VK_FORMAT_R16G16B16_UINT
   #:VK_FORMAT_R16G16B16_SSCALED
   #:VK_FORMAT_R16G16B16_USCALED
   #:VK_FORMAT_R16G16B16_SNORM
   #:VK_FORMAT_R16G16B16_UNORM
   #:VK_FORMAT_R16G16_SFLOAT
   #:VK_FORMAT_R16G16_SINT
   #:VK_FORMAT_R16G16_UINT
   #:VK_FORMAT_R16G16_SSCALED
   #:VK_FORMAT_R16G16_USCALED
   #:VK_FORMAT_R16G16_SNORM
   #:VK_FORMAT_R16G16_UNORM
   #:VK_FORMAT_R16_SFLOAT
   #:VK_FORMAT_R16_SINT
   #:VK_FORMAT_R16_UINT
   #:VK_FORMAT_R16_SSCALED
   #:VK_FORMAT_R16_USCALED
   #:VK_FORMAT_R16_SNORM
   #:VK_FORMAT_R16_UNORM
   #:VK_FORMAT_A2B10G10R10_SINT_PACK32
   #:VK_FORMAT_A2B10G10R10_UINT_PACK32
   #:VK_FORMAT_A2B10G10R10_SSCALED_PACK32
   #:VK_FORMAT_A2B10G10R10_USCALED_PACK32
   #:VK_FORMAT_A2B10G10R10_SNORM_PACK32
   #:VK_FORMAT_A2B10G10R10_UNORM_PACK32
   #:VK_FORMAT_A2R10G10B10_SINT_PACK32
   #:VK_FORMAT_A2R10G10B10_UINT_PACK32
   #:VK_FORMAT_A2R10G10B10_SSCALED_PACK32
   #:VK_FORMAT_A2R10G10B10_USCALED_PACK32
   #:VK_FORMAT_A2R10G10B10_SNORM_PACK32
   #:VK_FORMAT_A2R10G10B10_UNORM_PACK32
   #:VK_FORMAT_A8B8G8R8_SRGB_PACK32
   #:VK_FORMAT_A8B8G8R8_SINT_PACK32
   #:VK_FORMAT_A8B8G8R8_UINT_PACK32
   #:VK_FORMAT_A8B8G8R8_SSCALED_PACK32
   #:VK_FORMAT_A8B8G8R8_USCALED_PACK32
   #:VK_FORMAT_A8B8G8R8_SNORM_PACK32
   #:VK_FORMAT_A8B8G8R8_UNORM_PACK32
   #:VK_FORMAT_B8G8R8A8_SRGB
   #:VK_FORMAT_B8G8R8A8_SINT
   #:VK_FORMAT_B8G8R8A8_UINT
   #:VK_FORMAT_B8G8R8A8_SSCALED
   #:VK_FORMAT_B8G8R8A8_USCALED
   #:VK_FORMAT_B8G8R8A8_SNORM
   #:VK_FORMAT_B8G8R8A8_UNORM
   #:VK_FORMAT_R8G8B8A8_SRGB
   #:VK_FORMAT_R8G8B8A8_SINT
   #:VK_FORMAT_R8G8B8A8_UINT
   #:VK_FORMAT_R8G8B8A8_SSCALED
   #:VK_FORMAT_R8G8B8A8_USCALED
   #:VK_FORMAT_R8G8B8A8_SNORM
   #:VK_FORMAT_R8G8B8A8_UNORM
   #:VK_FORMAT_B8G8R8_SRGB
   #:VK_FORMAT_B8G8R8_SINT
   #:VK_FORMAT_B8G8R8_UINT
   #:VK_FORMAT_B8G8R8_SSCALED
   #:VK_FORMAT_B8G8R8_USCALED
   #:VK_FORMAT_B8G8R8_SNORM
   #:VK_FORMAT_B8G8R8_UNORM
   #:VK_FORMAT_R8G8B8_SRGB
   #:VK_FORMAT_R8G8B8_SINT
   #:VK_FORMAT_R8G8B8_UINT
   #:VK_FORMAT_R8G8B8_SSCALED
   #:VK_FORMAT_R8G8B8_USCALED
   #:VK_FORMAT_R8G8B8_SNORM
   #:VK_FORMAT_R8G8B8_UNORM
   #:VK_FORMAT_R8G8_SRGB
   #:VK_FORMAT_R8G8_SINT
   #:VK_FORMAT_R8G8_UINT
   #:VK_FORMAT_R8G8_SSCALED
   #:VK_FORMAT_R8G8_USCALED
   #:VK_FORMAT_R8G8_SNORM
   #:VK_FORMAT_R8G8_UNORM
   #:VK_FORMAT_R8_SRGB
   #:VK_FORMAT_R8_SINT
   #:VK_FORMAT_R8_UINT
   #:VK_FORMAT_R8_SSCALED
   #:VK_FORMAT_R8_USCALED
   #:VK_FORMAT_R8_SNORM
   #:VK_FORMAT_R8_UNORM
   #:VK_FORMAT_A1R5G5B5_UNORM_PACK16
   #:VK_FORMAT_B5G5R5A1_UNORM_PACK16
   #:VK_FORMAT_R5G5B5A1_UNORM_PACK16
   #:VK_FORMAT_B5G6R5_UNORM_PACK16
   #:VK_FORMAT_R5G6B5_UNORM_PACK16
   #:VK_FORMAT_B4G4R4A4_UNORM_PACK16
   #:VK_FORMAT_R4G4B4A4_UNORM_PACK16
   #:VK_FORMAT_R4G4_UNORM_PACK8
   #:VK_FORMAT_UNDEFINED
   #:VK_FILTER_CUBIC_IMG
   #:VK_FILTER_LINEAR
   #:VK_FILTER_NEAREST
   #:VK_FENCE_CREATE_SIGNALED_BIT
   #:VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_FENCE_FD_BIT_KHX
   #:VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHX
   #:VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHX
   #:VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHX
   #:VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHX
   #:VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHX
   #:VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHX
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHX
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHX
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHX
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHX
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHX
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHX
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHX
   #:VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV
   #:VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV
   #:VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV
   #:VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHX
   #:VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHX
   #:VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHX
   #:VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT
   #:VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV
   #:VK_DYNAMIC_STATE_STENCIL_REFERENCE
   #:VK_DYNAMIC_STATE_STENCIL_WRITE_MASK
   #:VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK
   #:VK_DYNAMIC_STATE_DEPTH_BOUNDS
   #:VK_DYNAMIC_STATE_BLEND_CONSTANTS
   #:VK_DYNAMIC_STATE_DEPTH_BIAS
   #:VK_DYNAMIC_STATE_LINE_WIDTH
   #:VK_DYNAMIC_STATE_SCISSOR
   #:VK_DYNAMIC_STATE_VIEWPORT
   #:VK_DISPLAY_POWER_STATE_ON_EXT
   #:VK_DISPLAY_POWER_STATE_SUSPEND_EXT
   #:VK_DISPLAY_POWER_STATE_OFF_EXT
   #:VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR
   #:VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR
   #:VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR
   #:VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR
   #:VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT
   #:VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT
   #:VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT
   #:VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX
   #:VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX
   #:VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX
   #:VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX
   #:VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT
   #:VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
   #:VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR
   #:VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT
   #:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC
   #:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
   #:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER
   #:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
   #:VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
   #:VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
   #:VK_DESCRIPTOR_TYPE_STORAGE_IMAGE
   #:VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE
   #:VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
   #:VK_DESCRIPTOR_TYPE_SAMPLER
   #:VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR
   #:VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
   #:VK_DEPENDENCY_DEVICE_GROUP_BIT_KHX
   #:VK_DEPENDENCY_VIEW_LOCAL_BIT_KHX
   #:VK_DEPENDENCY_BY_REGION_BIT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT
   #:VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT
   #:VK_DEBUG_REPORT_DEBUG_BIT_EXT
   #:VK_DEBUG_REPORT_ERROR_BIT_EXT
   #:VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT
   #:VK_DEBUG_REPORT_WARNING_BIT_EXT
   #:VK_DEBUG_REPORT_INFORMATION_BIT_EXT
   #:VK_CULL_MODE_FRONT_AND_BACK
   #:VK_CULL_MODE_BACK_BIT
   #:VK_CULL_MODE_FRONT_BIT
   #:VK_CULL_MODE_NONE
   #:VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR
   #:VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR
   #:VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR
   #:VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
   #:VK_COMPONENT_SWIZZLE_A
   #:VK_COMPONENT_SWIZZLE_B
   #:VK_COMPONENT_SWIZZLE_G
   #:VK_COMPONENT_SWIZZLE_R
   #:VK_COMPONENT_SWIZZLE_ONE
   #:VK_COMPONENT_SWIZZLE_ZERO
   #:VK_COMPONENT_SWIZZLE_IDENTITY
   #:VK_COMPARE_OP_ALWAYS
   #:VK_COMPARE_OP_GREATER_OR_EQUAL
   #:VK_COMPARE_OP_NOT_EQUAL
   #:VK_COMPARE_OP_GREATER
   #:VK_COMPARE_OP_LESS_OR_EQUAL
   #:VK_COMPARE_OP_EQUAL
   #:VK_COMPARE_OP_LESS
   #:VK_COMPARE_OP_NEVER
   #:VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT
   #:VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
   #:VK_COMMAND_POOL_CREATE_TRANSIENT_BIT
   #:VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
   #:VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT
   #:VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
   #:VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT
   #:VK_COMMAND_BUFFER_LEVEL_SECONDARY
   #:VK_COMMAND_BUFFER_LEVEL_PRIMARY
   #:VK_COLOR_SPACE_PASS_THROUGH_EXT
   #:VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT
   #:VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT
   #:VK_COLOR_SPACE_HDR10_HLG_EXT
   #:VK_COLOR_SPACE_DOLBYVISION_EXT
   #:VK_COLOR_SPACE_HDR10_ST2084_EXT
   #:VK_COLOR_SPACE_BT2020_LINEAR_EXT
   #:VK_COLOR_SPACE_BT709_NONLINEAR_EXT
   #:VK_COLOR_SPACE_BT709_LINEAR_EXT
   #:VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT
   #:VK_COLOR_SPACE_DCI_P3_LINEAR_EXT
   #:VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT
   #:VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT
   #:VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
   #:VK_COLOR_COMPONENT_A_BIT
   #:VK_COLOR_COMPONENT_B_BIT
   #:VK_COLOR_COMPONENT_G_BIT
   #:VK_COLOR_COMPONENT_R_BIT
   #:VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT
   #:VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
   #:VK_BUFFER_USAGE_INDEX_BUFFER_BIT
   #:VK_BUFFER_USAGE_STORAGE_BUFFER_BIT
   #:VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
   #:VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT
   #:VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT
   #:VK_BUFFER_USAGE_TRANSFER_DST_BIT
   #:VK_BUFFER_USAGE_TRANSFER_SRC_BIT
   #:VK_BUFFER_CREATE_SPARSE_ALIASED_BIT
   #:VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT
   #:VK_BUFFER_CREATE_SPARSE_BINDING_BIT
   #:VK_BORDER_COLOR_INT_OPAQUE_WHITE
   #:VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE
   #:VK_BORDER_COLOR_INT_OPAQUE_BLACK
   #:VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK
   #:VK_BORDER_COLOR_INT_TRANSPARENT_BLACK
   #:VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK
   #:VK_BLEND_OP_MAX
   #:VK_BLEND_OP_MIN
   #:VK_BLEND_OP_REVERSE_SUBTRACT
   #:VK_BLEND_OP_SUBTRACT
   #:VK_BLEND_OP_ADD
   #:VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA
   #:VK_BLEND_FACTOR_SRC1_ALPHA
   #:VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR
   #:VK_BLEND_FACTOR_SRC1_COLOR
   #:VK_BLEND_FACTOR_SRC_ALPHA_SATURATE
   #:VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA
   #:VK_BLEND_FACTOR_CONSTANT_ALPHA
   #:VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR
   #:VK_BLEND_FACTOR_CONSTANT_COLOR
   #:VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA
   #:VK_BLEND_FACTOR_DST_ALPHA
   #:VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
   #:VK_BLEND_FACTOR_SRC_ALPHA
   #:VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR
   #:VK_BLEND_FACTOR_DST_COLOR
   #:VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR
   #:VK_BLEND_FACTOR_SRC_COLOR
   #:VK_BLEND_FACTOR_ONE
   #:VK_BLEND_FACTOR_ZERO
   #:VK_ATTACHMENT_STORE_OP_DONT_CARE
   #:VK_ATTACHMENT_STORE_OP_STORE
   #:VK_ATTACHMENT_LOAD_OP_DONT_CARE
   #:VK_ATTACHMENT_LOAD_OP_CLEAR
   #:VK_ATTACHMENT_LOAD_OP_LOAD
   #:VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT
   #:VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX
   #:VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX
   #:VK_ACCESS_MEMORY_WRITE_BIT
   #:VK_ACCESS_MEMORY_READ_BIT
   #:VK_ACCESS_HOST_WRITE_BIT
   #:VK_ACCESS_HOST_READ_BIT
   #:VK_ACCESS_TRANSFER_WRITE_BIT
   #:VK_ACCESS_TRANSFER_READ_BIT
   #:VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
   #:VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT
   #:VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
   #:VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
   #:VK_ACCESS_SHADER_WRITE_BIT
   #:VK_ACCESS_SHADER_READ_BIT
   #:VK_ACCESS_INPUT_ATTACHMENT_READ_BIT
   #:VK_ACCESS_UNIFORM_READ_BIT
   #:VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT
   #:VK_ACCESS_INDEX_READ_BIT
   #:VK_ACCESS_INDIRECT_COMMAND_READ_BIT
   #:VK_SWAPCHAIN_CREATE_BIND_SFR_BIT_KHX
   #:VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR
   #:VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR
   #:VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR
   #:VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR
   #:VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR
   #:VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR
   #:VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR
   #:VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR
   #:VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
   #:VK_SURFACE_COUNTER_VBLANK_EXT
   #:VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX
   #:VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX
   #:VK_STENCIL_FRONT_AND_BACK
   #:VK_STENCIL_FACE_BACK_BIT
   #:VK_STENCIL_FACE_FRONT_BIT
   #:VK_SPARSE_MEMORY_BIND_METADATA_BIT
   #:VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT
   #:VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT
   #:VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT
   #:VK_SHADER_STAGE_ALL
   #:VK_SHADER_STAGE_ALL_GRAPHICS
   #:VK_SHADER_STAGE_COMPUTE_BIT
   #:VK_SHADER_STAGE_FRAGMENT_BIT
   #:VK_SHADER_STAGE_GEOMETRY_BIT
   #:VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT
   #:VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT
   #:VK_SHADER_STAGE_VERTEX_BIT
   #:VK_SAMPLE_COUNT_64_BIT
   #:VK_SAMPLE_COUNT_32_BIT
   #:VK_SAMPLE_COUNT_16_BIT
   #:VK_SAMPLE_COUNT_8_BIT
   #:VK_SAMPLE_COUNT_4_BIT
   #:VK_SAMPLE_COUNT_2_BIT
   #:VK_SAMPLE_COUNT_1_BIT
   #:VK_QUEUE_SPARSE_BINDING_BIT
   #:VK_QUEUE_TRANSFER_BIT
   #:VK_QUEUE_COMPUTE_BIT
   #:VK_QUEUE_GRAPHICS_BIT
   #:VK_QUERY_RESULT_PARTIAL_BIT
   #:VK_QUERY_RESULT_WITH_AVAILABILITY_BIT
   #:VK_QUERY_RESULT_WAIT_BIT
   #:VK_QUERY_RESULT_64_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT
   #:VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT
   #:VK_QUERY_CONTROL_PRECISE_BIT
   #:VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX
   #:VK_PIPELINE_STAGE_ALL_COMMANDS_BIT
   #:VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT
   #:VK_PIPELINE_STAGE_HOST_BIT
   #:VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
   #:VK_PIPELINE_STAGE_TRANSFER_BIT
   #:VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT
   #:VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
   #:VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
   #:VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
   #:VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
   #:VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
   #:VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
   #:VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT
   #:VK_PIPELINE_STAGE_VERTEX_SHADER_BIT
   #:VK_PIPELINE_STAGE_VERTEX_INPUT_BIT
   #:VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT
   #:VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
   #:VK_PIPELINE_CREATE_DISPATCH_BASE_KHX
   #:VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHX
   #:VK_PIPELINE_CREATE_DERIVATIVE_BIT
   #:VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT
   #:VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT
   #:VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX
   #:VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX
   #:VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX
   #:VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX
   #:VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX
   #:VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX
   #:VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT
   #:VK_MEMORY_PROPERTY_HOST_CACHED_BIT
   #:VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
   #:VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
   #:VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
   #:VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHX
   #:VK_MEMORY_HEAP_DEVICE_LOCAL_BIT
   #:VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX
   #:VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX
   #:VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX
   #:VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX
   #:VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX
   #:VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT
   #:VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
   #:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
   #:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
   #:VK_IMAGE_USAGE_STORAGE_BIT
   #:VK_IMAGE_USAGE_SAMPLED_BIT
   #:VK_IMAGE_USAGE_TRANSFER_DST_BIT
   #:VK_IMAGE_USAGE_TRANSFER_SRC_BIT
   #:VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR
   #:VK_IMAGE_CREATE_BIND_SFR_BIT_KHX
   #:VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT
   #:VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT
   #:VK_IMAGE_CREATE_SPARSE_ALIASED_BIT
   #:VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
   #:VK_IMAGE_CREATE_SPARSE_BINDING_BIT
   #:VK_IMAGE_ASPECT_METADATA_BIT
   #:VK_IMAGE_ASPECT_STENCIL_BIT
   #:VK_IMAGE_ASPECT_DEPTH_BIT
   #:VK_IMAGE_ASPECT_COLOR_BIT
   #:VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR
   #:VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR
   #:VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG
   #:VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT
   #:VK_FORMAT_FEATURE_BLIT_DST_BIT
   #:VK_FORMAT_FEATURE_BLIT_SRC_BIT
   #:VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT
   #:VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT
   #:VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT
   #:VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT
   #:VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT
   #:VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT
   #:VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT
   #:VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT
   #:VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT
   #:VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT
   #:VK_FENCE_CREATE_SIGNALED_BIT
   #:VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_FENCE_FD_BIT_KHX
   #:VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHX
   #:VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHX
   #:VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHX
   #:VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHX
   #:VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHX
   #:VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHX
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHX
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHX
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHX
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHX
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHX
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHX
   #:VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHX
   #:VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV
   #:VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV
   #:VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV
   #:VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHX
   #:VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHX
   #:VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHX
   #:VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR
   #:VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR
   #:VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR
   #:VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR
   #:VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX
   #:VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX
   #:VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX
   #:VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX
   #:VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR
   #:VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
   #:VK_DEPENDENCY_DEVICE_GROUP_BIT_KHX
   #:VK_DEPENDENCY_VIEW_LOCAL_BIT_KHX
   #:VK_DEPENDENCY_BY_REGION_BIT
   #:VK_DEBUG_REPORT_DEBUG_BIT_EXT
   #:VK_DEBUG_REPORT_ERROR_BIT_EXT
   #:VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT
   #:VK_DEBUG_REPORT_WARNING_BIT_EXT
   #:VK_DEBUG_REPORT_INFORMATION_BIT_EXT
   #:VK_CULL_MODE_FRONT_AND_BACK
   #:VK_CULL_MODE_BACK_BIT
   #:VK_CULL_MODE_FRONT_BIT
   #:VK_CULL_MODE_NONE
   #:VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR
   #:VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR
   #:VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR
   #:VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
   #:VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT
   #:VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
   #:VK_COMMAND_POOL_CREATE_TRANSIENT_BIT
   #:VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
   #:VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT
   #:VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
   #:VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT
   #:VK_COLOR_COMPONENT_A_BIT
   #:VK_COLOR_COMPONENT_B_BIT
   #:VK_COLOR_COMPONENT_G_BIT
   #:VK_COLOR_COMPONENT_R_BIT
   #:VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT
   #:VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
   #:VK_BUFFER_USAGE_INDEX_BUFFER_BIT
   #:VK_BUFFER_USAGE_STORAGE_BUFFER_BIT
   #:VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
   #:VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT
   #:VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT
   #:VK_BUFFER_USAGE_TRANSFER_DST_BIT
   #:VK_BUFFER_USAGE_TRANSFER_SRC_BIT
   #:VK_BUFFER_CREATE_SPARSE_ALIASED_BIT
   #:VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT
   #:VK_BUFFER_CREATE_SPARSE_BINDING_BIT
   #:VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT
   #:VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX
   #:VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX
   #:VK_ACCESS_MEMORY_WRITE_BIT
   #:VK_ACCESS_MEMORY_READ_BIT
   #:VK_ACCESS_HOST_WRITE_BIT
   #:VK_ACCESS_HOST_READ_BIT
   #:VK_ACCESS_TRANSFER_WRITE_BIT
   #:VK_ACCESS_TRANSFER_READ_BIT
   #:VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
   #:VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT
   #:VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
   #:VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
   #:VK_ACCESS_SHADER_WRITE_BIT
   #:VK_ACCESS_SHADER_READ_BIT
   #:VK_ACCESS_INPUT_ATTACHMENT_READ_BIT
   #:VK_ACCESS_UNIFORM_READ_BIT
   #:VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT
   #:VK_ACCESS_INDEX_READ_BIT
   #:VK_ACCESS_INDIRECT_COMMAND_READ_BIT))
