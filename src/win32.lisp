(in-package :vk)

#|
// Provided by VK_KHR_win32_surface
typedef struct VkWin32SurfaceCreateInfoKHR {
    VkStructureType                 sType;
    const void*                     pNext;
    VkWin32SurfaceCreateFlagsKHR    flags;
    HINSTANCE                       hinstance;
    HWND                            hwnd;
} VkWin32SurfaceCreateInfoKHR;
|#

(cffi:defcstruct VkWin32SurfaceCreateInfoKHR
  (sType %vk::vkStructureType)
  (pNext :pointer)
  (flags :int)
  (hinstance :pointer)
  (hwnd :pointer))

(defconstant VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR 1000009000)

(defun create-win32-window-surface (instance window allocator)
  (let ((p-fn-vkCreateWin32SurfaceKHR
	  (cffi:with-foreign-string (pstr "vkCreateWin32SurfaceKHR")
	    (%vk:vkGetInstanceProcAddr (h instance) pstr)))
	(err nil))

    (when (cffi:null-pointer-p p-fn-vkCreateWin32SurfaceKHR)
      (error "Win32: Vulkan instance missing VK_KHR_win32_surface extension."))

    (cffi:with-foreign-object (p-info '(:struct VKWin32SurfaceCreateInfoKHR))
      (cffi:with-foreign-slots ((sType
				 pNext
				 flags
				 hinstance
				 hwnd)
				p-info
				(:struct VKWin32SurfaceCreateInfoKHR))

	(setf sType VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR
	      pNext (cffi:null-pointer)
	      flags 0
	      hinstance (h instance)
	      hwnd (noffi::ptr-value (h window)))

	(cffi:with-foreign-object (p-surface 'vk::VkSurfaceKHR)
	  (setq err (cffi:foreign-funcall-pointer p-fn-vkCreateWin32SurfaceKHR ()
						  :pointer (h instance)
						  :pointer p-info
						  :pointer (h allocator)
						  :pointer p-surface
						  :int))

	  (vk:check-vk-result err)

	  (make-instance 'surface
			 :handle (cffi:mem-aref p-surface 'vk::VkSurfaceKHR)
			 :window window
			 :allocator allocator))))))
