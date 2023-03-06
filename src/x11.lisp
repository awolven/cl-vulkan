(in-package :vk)

#|
// Provided by VK_KHR_xcb_surface
typedef struct VkXcbSurfaceCreateInfoKHR {
    VkStructureType               sType;
    const void*                   pNext;
    VkXcbSurfaceCreateFlagsKHR    flags;
    xcb_connection_t*             connection;
    xcb_window_t                  window;
} VkXcbSurfaceCreateInfoKHR;
|#

(cffi:defcstruct VkXcbSurfaceCreateInfoKHR
  (sType %vk::vkStructureType)
  (pNext :pointer)
  (flags :int)
  (connection :pointer)
  (window :uint32))

(defconstant VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR 1000005000)

#|
typedef struct VkXlibSurfaceCreateInfoKHR {
    VkStructureType                sType;
    const void*                    pNext;
    VkXlibSurfaceCreateFlagsKHR    flags;
    Display*                       dpy;
    Window                         window;
} VkXlibSurfaceCreateInfoKHR;
|#

(cffi:defcstruct VkXlibSurfaceCreateInfoKHR
  (sType %vk::vkStructureType)
  (pNext :pointer)
  (flags :int)
  (dpy :pointer)
  (window :uint32))

(defconstant VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR 1000004000)

#+noglfw
(defun create-x11-window-surface (display instance window allocator)
  (let ((err))
    (cffi:with-foreign-object (p-surface 'vk::VkSurfaceKHR)
      
      (if (and (clui::xcb-available? (clui::display-x11-state display))
	       (clui::xcb-vulkan-surface? (clui::display-x11-state display)))
	  
	  (let ((xcb-connection (#_XGetXCBConnection (h display))))
	    (unless xcb-connection
	      (error "X11: Failed to retrieve XCB connection."))

	    (let ((p-fn-vkCreateXCBSurfaceKHR
		    (cffi:with-foreign-string (pstr "vkCreateXcbSurfaceKHR")
		      (%vk:vkGetInstanceProcAddr (h instance) pstr))))

	      (cffi::with-foreign-object (p-info '(:struct VkXcbSurfaceCreateInfoKHR))
		(cffi:with-foreign-slots ((sType
					   pNext
					   flags
					   connection)
					  p-info
					  (:struct VkXcbSurfaceCreateInfoKHR))
		  (setf sType VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR
			pNext (cffi:null-pointer)
			flags 0
			connection (noffi::ptr-value xcb-connection)))
		  (setf (cffi:foreign-slot-value p-info '(:struct VkXcbSurfaceCreateInfoKHR) 'window) (h window))

		  (setq err (cffi:foreign-funcall-pointer p-fn-vkCreateXcbSurfaceKHR ()
							    :pointer (h instance)
							    :pointer p-info
							    :pointer (h allocator)
							    :pointer p-surface
							    :int)))))

	  (let ((p-fn-vkCreateXlibSurfaceKHR
		  (cffi:with-foreign-string (pstr "vkCreateXlibSurfaceKHR")
		    (%vk:vkGetInstanceProcAddr (h instance) pstr))))

	    (cffi::with-foreign-object (p-info '(:struct VkXlibSurfaceCreateInfoKHR))
	      (cffi:with-foreign-slots ((sType
					 pNext
					 flags
					 dpy)
					p-info
					(:struct VkXlibSurfaceCreateInfoKHR))
		(setf sType VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR
		      pNext (cffi:null-pointer)
		      flags 0
		      dpy (noffi::ptr-value (h display))))
		(setf (cffi:foreign-slot-value p-info '(:struct VkXlibSurfaceCreateInfoKHR) 'window) (h window))

		(setq err (cffi:foreign-funcall-pointer p-fn-vkCreateXlibSurfaceKHR ()
							:pointer (h instance)
							:pointer p-info
							:pointer (h allocator)
							:pointer p-surface
							:int)))))


      (vk:check-vk-result err)
      
      (make-instance 'surface
		     :handle (cffi:mem-aref p-surface 'vk::VkSurfaceKHR)
		     :window window
		     :allocator allocator))))


      
