(in-package :vk)

(defun get-cocoa-required-instance-extensions ()
  (list "VK_KHR_surface"
	(if (symbol-value (intern (symbol-name '*use-metal-surface*) :vk))
	    "VK_EXT_metal_surface"
	    "VK_MVK_macos_surface")))

#+objc
(named-readtables:in-readtable :objc-readtable)

#|
typedef struct VkMetalSurfaceCreateInfoEXT {
    VkStructureType                 sType;
    const void*                     pNext;
    VkMetalSurfaceCreateFlagsEXT    flags;
    const CAMetalLayer*             pLayer;
} VkMetalSurfaceCreateInfoEXT;
|#

(cffi:defcstruct %vk::VkMetalSurfaceCreateInfoEXT
  (%vk::sType :int)
  (%vk::pNext :pointer)
  (%vk::flags :int)
  (%vk::pLayer :pointer))

(defconstant %vk::VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT 1000217000)
#|
typedef struct VkMacOSSurfaceCreateInfoMVK {
    VkStructureType                 sType;
    const void*                     pNext;
    VkMacOSSurfaceCreateFlagsMVK    flags;
    const void*                     pView;
} VkMacOSSurfaceCreateInfoMVK;
|#

(cffi:defcstruct %vk::VkMacOSSurfaceCreateInfoMVK
  (%vk::sType :int)
  (%vk::pNext :pointer)
  (%vk::flags :int)
  (%vk::pView :pointer))

(defconstant %vk::VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK 1000123000)

(defvar *use-metal-surface* nil)

#+noglfw
(defun create-cocoa-window-surface (window allocator)
  (let ((instance (get-vulkan-instance nil)))
    (with-foreign-object (p-surface 'VkSurfaceKHR)
      (let ((bundle (ns::|bundleWithPath:| #@NSBundle
			 (objc-runtime::make-nsstring "/System/Library/Frameworks/QuartzCore.framework"))))
	(when (cffi:null-pointer-p bundle)
	  (error "Cocoa: Failed to find QuartzCore.framework"))

	(setf (clui::window-layer window)
	      (ns::|layer| (ns::|classNamed:| bundle (objc-runtime::make-nsstring "CAMetalLayer"))))

	(when (cffi:null-pointer-p (clui::window-layer window))
	  (error "Cocoa: Failed to create layer for view."))
	;;(when (abstract-os::window-retina? window)
      
	(ns::|setContentsScale:| (clui::window-layer window) (ns::|backingScaleFactor| window))
      
	(ns::|setLayer:| (clui::window-content-view window) (clui::window-layer window))
	(ns::|setWantsLayer:| (clui::window-content-view window) t)
      
	(let ((err))
	  (if *use-metal-surface*
	      (let ((p-fn-vkCreateMetalSurfaceEXT
		     (with-foreign-string (pstr "vkCreateMetalSurfaceEXT")
		       (%vk:vkGetInstanceProcAddr (h instance) pstr))))
	      
		(when (cffi:null-pointer-p p-fn-vkCreateMetalSurfaceEXT)
		  (warn "Cocoa: Vulkan instance missing VK_EXT_metal_surface extension"))
	      
		(with-foreign-object (sci '(:struct %vk::VkMetalSurfaceCreateInfoEXT))
		  (%vk:zero-struct sci '(:struct %vk::VkMetalSurfaceCreateInfoEXT))
		  (with-foreign-slots ((%vk::sType
					%vk::pLayer)
				       sci (:struct %vk::VkMetalSurfaceCreateInfoEXT))
		    (setf %vk::sType %vk::VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT
			  %vk::pLayer (clui::window-layer window))
		    (setq err (cffi:foreign-funcall-pointer p-fn-vkCreateMetalSurfaceEXT ()
							    :pointer (h instance)
							    :pointer sci
							    :pointer (h allocator)
							    :pointer p-surface
							    :int)))))
	      (let ((p-fn-vkCreateMacOSSurfaceMVK
		     (with-foreign-string (pstr "vkCreateMacOSSurfaceMVK")
		       (%vk:vkGetInstanceProcAddr (h instance) pstr))))
	      
		(when (cffi:null-pointer-p p-fn-vkCreateMacOSSurfaceMVK)
		  (error "Cocoa: Vulkan instance missing VK_MVK_macos_surface extension"))
	      
		(with-foreign-object (sci '(:struct %vk::VkMacOSSurfaceCreateInfoMVK))
		  (%vk:zero-struct sci '(:struct %vk::VkMacOSSurfaceCreateInfoMVK))
		  (with-foreign-slots ((%vk::sType
					%VK::pView)
				       sci (:struct %vk::VkMacOSSurfaceCreateInfoMVK))
		    (setf %vk::sType %vk::VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK
			  %vk::pView (clui::objc-object-id (clui::window-content-view window)))
		    (setq err (cffi:foreign-funcall-pointer p-fn-vkCreateMacOSSurfaceMVK ()
							    :pointer (h instance)
							    :pointer sci
							    :pointer (h allocator)
							    :pointer p-surface
							    :int))))))
	  (vk:check-vk-result err)

	  (make-instance 'surface
			 :handle (mem-aref p-surface 'VkSurfaceKHR)
			 :window window
			 :allocator allocator))))))
