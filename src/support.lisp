;; Copyright 2019 Andrew Kenneth Wolven
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

(cffi:defcfun ("memcpy" memcpy) :pointer
  (dest :pointer)
  (src :pointer)
  (count size-t))

(defcfun ("memset" memset) :pointer
  (str :pointer) (ch :int) (n size-t))

(%vk::defvkdevextfun ("vkCmdSetLineStippleEXT" vkCmdSetLineStippleEXT) :void
  (command-buffer VkCommandBuffer)
  (line-stipple-factor :unsigned-int)
  (line-stipple-pattern :unsigned-short))

(defvar *debug* (if (boundp 'cl-user::*debug*) (symbol-value 'cl-user::*debug*) 1))
  
(defconstant VK_UUID_SIZE 16)

(defconstant +NULL+ 0)
;;(defconstant IMGUI_MAX_POSSIBLE_BACK_BUFFERS 16)
(defconstant UINT64_MAX #.(1- (expt 2 64)))

(defconstant VK_WHOLE_SIZE #.(1- (expt 2 64)))

(defparameter +nullptr+ (cffi-sys:null-pointer))

(defparameter VK_NULL_HANDLE +nullptr+)
;;(defparameter *imgui-unlimited-frame-rate* nil)
(defconstant VK_QUEUE_FAMILY_IGNORED 0)
(defconstant VK_IMAGE_LAYOUT_UNDEFINED 0)
(defconstant VK_SUBPASS_EXTERNAL 0)

(defparameter VK_KHR_SURFACE_EXTENSION_NAME "VK_KHR_surface")
(defparameter VK_KHR_SWAPCHAIN_EXTENSION_NAME "VK_KHR_swapchain")
(defparameter VK_KHR_DISPLAY_EXTENSION_NAME "VK_KHR_display")
(defparameter VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME "VK_KHR_display_swapchain")
(defparameter VK_KHR_XLIB_SURFACE_EXTENSION_NAME "VK_KHR_xlib_surface")
(defparameter VK_KHR_XCB_SURFACE_EXTENSION_NAME "VK_KHR_xcb_surface")
(defparameter VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME "VK_KHR_wayland_surface")
(defparameter VK_KHR_MIR_SURFACE_EXTENSION_NAME "VK_KHR_mir_surface")
(defparameter VK_KHR_ANDROID_SURFACE_EXTENSION_NAME "VK_KHR_android_surface")
(defparameter VK_KHR_WIN32_SURFACE_EXTENSION_NAME "VK_KHR_win32_surface")
(defparameter VK_EXT_DEBUG_REPORT_EXTENSION_NAME "VK_EXT_debug_report")
(defparameter VK_NV_GLSL_SHADER_EXTENSION_NAME "VK_NV_glsl_shader")
(defparameter VK_NV_EXTENSION_1_EXTENSION_NAME "VK_NV_extension_1")
(defparameter VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME "VK_KHR_sampler_mirror_clamp_to_edge")
(defparameter VK_IMG_FILTER_CUBIC_EXTENSION_NAME "VK_IMG_filter_cubic")
(defparameter VK_AMD_EXTENSION_17_EXTENSION_NAME "VK_AMD_extension_17")
(defparameter VK_AMD_EXTENSION_18_EXTENSION_NAME "VK_AMD_extension_18")
(defparameter VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME "VK_AMD_rasterization_order")
(defparameter VK_AMD_EXTENSION_20_EXTENSION_NAME "VK_AMD_extension_20")
(defparameter VK_AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME "VK_AMD_shader_trinary_minmax")
(defparameter VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME "VK_AMD_shader_explicit_vertex_parameter")
(defparameter VK_EXT_DEBUG_MARKER_EXTENSION_NAME "VK_EXT_debug_marker")
(defparameter VK_AMD_EXTENSION_24_EXTENSION_NAME "VK_AMD_extension_24")
(defparameter VK_AMD_EXTENSION_25_EXTENSION_NAME "VK_AMD_extension_25")
(defparameter VK_AMD_GCN_SHADER_EXTENSION_NAME "VK_AMD_gcn_shader")
(defparameter VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME "VK_NV_dedicated_allocation")
(defparameter VK_NV_EXTENSION_28_EXTENSION_NAME "VK_NV_extension_28")
(defparameter VK_NVX_EXTENSION_29_EXTENSION_NAME "VK_NVX_extension_29")
(defparameter VK_NVX_EXTENSION_30_EXTENSION_NAME "VK_NVX_extension_30")
(defparameter VK_NVX_EXTENSION_31_EXTENSION_NAME "VK_NVX_extension_31")
(defparameter VK_AMD_EXTENSION_32_EXTENSION_NAME "VK_AMD_extension_32")
(defparameter VK_AMD_EXTENSION_33_EXTENSION_NAME "VK_AMD_extension_33")
(defparameter VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME "VK_AMD_draw_indirect_count")
(defparameter VK_AMD_EXTENSION_35_EXTENSION_NAME "VK_AMD_extension_35")
(defparameter VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME "VK_AMD_negative_viewport_height")
(defparameter VK_AMD_GPU_SHADER_HALF_FLOAT_EXTENSION_NAME "VK_AMD_gpu_shader_half_float")
(defparameter VK_AMD_SHADER_BALLOT_EXTENSION_NAME "VK_AMD_shader_ballot")
(defparameter VK_AMD_EXTENSION_39_EXTENSION_NAME "VK_AMD_extension_39")
(defparameter VK_AMD_EXTENSION_40_EXTENSION_NAME "VK_AMD_extension_40")
(defparameter VK_AMD_EXTENSION_41_EXTENSION_NAME "VK_AMD_extension_41")
(defparameter VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME "VK_AMD_texture_gather_bias_lod")
(defparameter VK_AMD_EXTENSION_43_EXTENSION_NAME "VK_AMD_extension_43")
(defparameter VK_AMD_EXTENSION_44_EXTENSION_NAME "VK_AMD_extension_44")
(defparameter VK_AMD_EXTENSION_45_EXTENSION_NAME "VK_AMD_extension_45")
(defparameter VK_AMD_EXTENSION_46_EXTENSION_NAME "VK_AMD_extension_46")
(defparameter VK_AMD_EXTENSION_47_EXTENSION_NAME "VK_AMD_extension_47")
(defparameter VK_NVX_EXTENSION_48_EXTENSION_NAME "VK_NVX_extension_48")
(defparameter VK_GOOGLE_EXTENSION_49_EXTENSION_NAME "VK_GOOGLE_extension_49")
(defparameter VK_GOOGLE_EXTENSION_50_EXTENSION_NAME "VK_GOOGLE_extension_50")
(defparameter VK_NVX_EXTENSION_51_EXTENSION_NAME "VK_NVX_extension_51")
(defparameter VK_NVX_EXTENSION_52_EXTENSION_NAME "VK_NVX_extension_52")
(defparameter VK_NV_EXTENSION_53_EXTENSION_NAME "VK_NV_extension_53")
(defparameter VK_KHX_MULTIVIEW_EXTENSION_NAME "VK_KHX_multiview")
(defparameter VK_IMG_FORMAT_PVRTC_EXTENSION_NAME "VK_IMG_format_pvrtc")
(defparameter VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME "VK_NV_external_memory_capabilities")
(defparameter VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME "VK_NV_external_memory")
(defparameter VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME "VK_NV_external_memory_win32")
(defparameter VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME "VK_NV_win32_keyed_mutex")
(defparameter VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES2_EXTENSION_NAME "VK_KHR_get_physical_device_properties2")
(defparameter VK_KHX_DEVICE_GROUP_EXTENSION_NAME "VK_KHX_device_group")
(defparameter VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME "VK_EXT_validation_flags")
(defparameter VK_NN_VI_SURFACE_EXTENSION_NAME "VK_NN_vi_surface")
(defparameter VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME "VK_KHR_shader_draw_parameters")
(defparameter VK_EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME "VK_EXT_shader_subgroup_ballot")
(defparameter VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME "VK_EXT_shader_subgroup_vote")
(defparameter VK_ARM_EXTENSION_01_EXTENSION_NAME "VK_ARM_extension_01")
(defparameter VK_ARM_EXTENSION_02_EXTENSION_NAME "VK_ARM_extension_02")
(defparameter VK_IMG_EXTENSION_69_EXTENSION_NAME "VK_IMG_extension_69")
(defparameter VK_KHR_MAINTENANCE1_EXTENSION_NAME "VK_KHR_maintenance1")
(defparameter VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME "VK_KHX_device_group_creation")
(defparameter VK_KHX_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME "VK_KHX_external_memory_capabilities")
(defparameter VK_KHX_EXTERNAL_MEMORY_EXTENSION_NAME "VK_KHX_external_memory")
(defparameter VK_KHX_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME "VK_KHX_external_memory_win32")
(defparameter VK_KHX_EXTERNAL_MEMORY_FD_EXTENSION_NAME "VK_KHX_external_memory_fd")
(defparameter VK_KHX_WIN32_KEYED_MUTEX_EXTENSION_NAME "VK_KHX_win32_keyed_mutex")
(defparameter VK_KHX_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME "VK_KHX_external_semaphore_capabilities")
(defparameter VK_KHX_EXTERNAL_SEMAPHORE_EXTENSION_NAME "VK_KHX_external_semaphore")
(defparameter VK_KHX_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME "VK_KHX_external_semaphore_win32")
(defparameter VK_KHX_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME "VK_KHX_external_semaphore_fd")
(defparameter VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME "VK_KHR_push_descriptor")
(defparameter VK_KHR_EXTENSION_82_EXTENSION_NAME "VK_KHR_extension_82")
(defparameter VK_KHR_EXTENSION_83_EXTENSION_NAME "VK_KHR_extension_83")
(defparameter VK_KHR_EXTENSION_84_EXTENSION_NAME "VK_KHR_extension_84")
(defparameter VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME "VK_KHR_incremental_present")
(defparameter VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME "VK_KHR_descriptor_update_template")
(defparameter VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME "VK_NVX_device_generated_commands")
(defparameter VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME "VK_NV_clip_space_w_scaling")
(defparameter VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME "VK_EXT_direct_mode_display")
(defparameter VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME "VK_EXT_acquire_xlib_display")
(defparameter VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME "VK_EXT_display_surface_counter")
(defparameter VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME "VK_EXT_display_control")
(defparameter VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME "VK_GOOGLE_display_timing")
(defparameter VK_KHR_EXTENSION_94_EXTENSION_NAME "VK_KHR_extension_94")
(defparameter VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME "VK_NV_sample_mask_override_coverage")
(defparameter VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME "VK_NV_geometry_shader_passthrough")
(defparameter VK_NV_VIEWPORT_ARRAY2_EXTENSION_NAME "VK_NV_viewport_array2")
(defparameter VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME "VK_NVX_multiview_per_view_attributes")
(defparameter VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME "VK_NV_viewport_swizzle")
(defparameter VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME "VK_EXT_discard_rectangles")
(defparameter VK_NV_EXTENSION_101_EXTENSION_NAME "VK_NV_extension_101")
(defparameter VK_NV_EXTENSION_102_EXTENSION_NAME "VK_NV_extension_102")
(defparameter VK_NV_EXTENSION_103_EXTENSION_NAME "VK_NV_extension_103")
(defparameter VK_NV_EXTENSION_104_EXTENSION_NAME "VK_NV_extension_104")
(defparameter VK_EXT_SWAPCHAIN_COLORSPACE_EXTENSION_NAME "VK_EXT_swapchain_colorspace")
(defparameter VK_EXT_HDR_METADATA_EXTENSION_NAME "VK_EXT_hdr_metadata")
(defparameter VK_IMG_EXTENSION_107_EXTENSION_NAME "VK_IMG_extension_107")
(defparameter VK_IMG_EXTENSION_108_EXTENSION_NAME "VK_IMG_extension_108")
(defparameter VK_IMG_EXTENSION_109_EXTENSION_NAME "VK_IMG_extension_109")
(defparameter VK_IMG_EXTENSION_110_EXTENSION_NAME "VK_IMG_extension_110")
(defparameter VK_IMG_EXTENSION_111_EXTENSION_NAME "VK_IMG_extension_111")
(defparameter VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME "VK_KHR_shared_presentable_image")
(defparameter VK_KHR_EXTENSION_113_EXTENSION_NAME "VK_KHR_extension_113")
(defparameter VK_KHR_EXTENSION_114_EXTENSION_NAME "VK_KHR_extension_114")
(defparameter VK_KHR_EXTENSION_115_EXTENSION_NAME "VK_KHR_extension_115")
(defparameter VK_KHR_EXTENSION_116_EXTENSION_NAME "VK_KHR_extension_116")
(defparameter VK_KHR_EXTENSION_117_EXTENSION_NAME "VK_KHR_extension_117")
(defparameter VK_KHR_EXTENSION_118_EXTENSION_NAME "VK_KHR_extension_118")
(defparameter VK_KHR_EXTENSION_119_EXTENSION_NAME "VK_KHR_extension_119")
(defparameter VK_KHR_GET_SURFACE_CAPABILITIES2_EXTENSION_NAME "VK_KHR_get_surface_capabilities2")
(defparameter VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME "VK_KHR_variable_pointers")
(defparameter VK_KHR_EXTENSION_122_EXTENSION_NAME "VK_KHR_extension_122")
(defparameter VK_MVK_IOS_SURFACE_EXTENSION_NAME "VK_MVK_ios_surface")
(defparameter VK_MVK_MACOS_SURFACE_EXTENSION_NAME "VK_MVK_macos_surface")
(defparameter VK_MVK_MOLTENVK_EXTENSION_NAME "VK_MVK_moltenvk")
(defparameter VK_MESA_EXTENSION_126_EXTENSION_NAME "VK_MESA_extension_126")
(defparameter VK_MESA_EXTENSION_127_EXTENSION_NAME "VK_MESA_extension_127")
(defparameter VK_KHR_EXTENSION_128_EXTENSION_NAME "VK_KHR_extension_128")
(defparameter VK_EXT_EXTENSION_129_EXTENSION_NAME "VK_EXT_extension_129")
(defparameter VK_KHR_EXTENSION_130_EXTENSION_NAME "VK_KHR_extension_130")
(defparameter VK_KHR_EXTENSION_131_EXTENSION_NAME "VK_KHR_extension_131")
(defparameter VK_KHR_EXTENSION_132_EXTENSION_NAME "VK_KHR_extension_132")
(defparameter VK_AMD_EXTENSION_133_EXTENSION_NAME "VK_AMD_extension_133")
(defparameter VK_AMD_EXTENSION_134_EXTENSION_NAME "VK_AMD_extension_134")
(defparameter VK_AMD_EXTENSION_135_EXTENSION_NAME "VK_AMD_extension_135")
(defparameter VK_AMD_EXTENSION_136_EXTENSION_NAME "VK_AMD_extension_136")
(defparameter VK_AMD_EXTENSION_137_EXTENSION_NAME "VK_AMD_extension_137")
(defparameter VK_AMD_EXTENSION_138_EXTENSION_NAME "VK_AMD_extension_138")
(defparameter VK_AMD_EXTENSION_139_EXTENSION_NAME "VK_AMD_extension_139")
(defparameter VK_AMD_EXTENSION_140_EXTENSION_NAME "VK_AMD_extension_140")
(defparameter VK_AMD_EXTENSION_141_EXTENSION_NAME "VK_AMD_extension_141")
(defparameter VK_AMD_EXTENSION_142_EXTENSION_NAME "VK_AMD_extension_142")
(defparameter VK_AMD_EXTENSION_143_EXTENSION_NAME "VK_AMD_extension_143")
(defparameter VK_AMD_EXTENSION_144_EXTENSION_NAME "VK_AMD_extension_144")
(defparameter VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME "VK_KHR_relaxed_block_layout")
(defparameter VK_KHR_EXTENSION_146_EXTENSION_NAME "VK_KHR_extension_146")
(defparameter VK_KHR_EXTENSION_147_EXTENSION_NAME "VK_KHR_extension_147")
(defparameter VK_EXT_EXTENSION_148_EXTENSION_NAME "VK_EXT_extension_148")
(defparameter VK_NV_EXTENSION_149_EXTENSION_NAME "VK_NV_extension_149")
(defparameter VK_NV_EXTENSION_150_EXTENSION_NAME "VK_NV_extension_150")
(defparameter VK_NV_EXTENSION_151_EXTENSION_NAME "VK_NV_extension_151")
(defparameter VK_NV_EXTENSION_152_EXTENSION_NAME "VK_NV_extension_152")
(defparameter VK_NV_EXTENSION_153_EXTENSION_NAME "VK_NV_extension_153")
(defparameter VK_NV_EXTENSION_154_EXTENSION_NAME "VK_NV_extension_154")
(defparameter VK_NV_EXTENSION_155_EXTENSION_NAME "VK_NV_extension_155")
(defparameter VK_NV_EXTENSION_156_EXTENSION_NAME "VK_NV_extension_156")
(defparameter VK_KHR_EXTENSION_157_EXTENSION_NAME "VK_KHR_extension_157")
(defparameter VK_KHR_EXTENSION_158_EXTENSION_NAME "VK_KHR_extension_158")
(defparameter VK_EXT_EXTENSION_159_EXTENSION_NAME "VK_EXT_extension_159")


(defun check-vk-result (result)
  (case result
    (#.VK_SUCCESS (values))
    (#.VK_NOT_READY   (warn "A fence or query has not yet completed.") (values))
    (#.VK_TIMEOUT     (warn "A wait operation has not completed in the specified time.") (values))
    (#.VK_EVENT_SET   (format *error-output* "An event is signaled.") (values))
    (#.VK_EVENT_RESET (format *error-output* "An even is unsignaled.") (values))
    (#.VK_INCOMPLETE  (warn "A return array was too small for the result.") (values))

    (#.VK_ERROR_OUT_OF_HOST_MEMORY
     (error "A host memory allocation has failed."))
    (#.VK_ERROR_OUT_OF_DEVICE_MEMORY
     (error "A device memory allocation has failed."))
    (#.VK_ERROR_INITIALIZATION_FAILED
     (error "Initialization of an object has failed."))
    (#.VK_ERROR_DEVICE_LOST
     (error "The logical device has been lost."))
    (#.VK_ERROR_MEMORY_MAP_FAILED
     (error "Mapping of a memory object has failed."))
    (#.VK_ERROR_LAYER_NOT_PRESENT
     (error "Layer specified does not exist."))
    (#.VK_ERROR_EXTENSION_NOT_PRESENT
     (error "Extension specified does not exist."))
    (#.VK_ERROR_FEATURE_NOT_PRESENT
     (error "Requested feature is not available on this device."))
    (#.VK_ERROR_INCOMPATIBLE_DRIVER
     (error "Unable to find vulkan driver."))
    (#.VK_ERROR_TOO_MANY_OBJECTS
     (error "Too many objects of this type have already been created."))
    (#.VK_ERROR_FORMAT_NOT_SUPPORTED
     (error "Requested format is not supported on this device."))
    (#.VK_ERROR_FRAGMENTED_POOL
     (error "A requested pool allocation has failed due to fragmentation of the pool's memory."))
    (#.VK_ERROR_SURFACE_LOST_KHR
     (error "VK_KHR_surface: VK_ERROR_SURFACE_LOST_KHR"))
    (#.VK_ERROR_NATIVE_WINDOW_IN_USE_KHR
     (error "VK_KHR_surface: VK_ERROR_NATIVE_WINDOW_IN_USE"))
    (#.VK_SUBOPTIMAL_KHR
     (warn "VK_KHR_swapchain: VK_SUBOPTIMAL_KHR") (values))
    (#.VK_ERROR_OUT_OF_DATE_KHR
     (error "VK_KHR_swapchain: VK_ERROR_OUT_OF_DATE_KHR"))
    (#.VK_ERROR_INCOMPATIBLE_DISPLAY_KHR
     (error "VK_KHR_display_swapchain: VK_INCOMPATIBLE_DISPLAY"))
    (#.VK_ERROR_VALIDATION_FAILED_EXT
     (error "VK_EXT_debug_report: VK_ERROR_VALIDATION_FAILED"))
    (#.VK_ERROR_INVALID_SHADER_NV
     (error "VK_NV_glsl_shader: VK_ERROR_INVALID_SHADER_NV"))
    (#.VK_NV_EXTENSION_1_ERROR
     (error "VK_NV_extension_1: VK_NV_EXTENSION_1_ERROR"))
    (#.VK_ERROR_OUT_OF_POOL_MEMORY_KHR
     (error "VK_KHR_maintenance1: VK_ERROR_OUT_OF_POOL_MEMORY_KHR"))
    (#.VK_ERROR_INVALID_EXTERNAL_HANDLE_KHX
     (error "VK_KHX_external_memory: VK_ERROR_INVALID_EXTERNAL_HANDLE_KHX"))
    (t (error "Unknown VkResult: ~S" result))))
