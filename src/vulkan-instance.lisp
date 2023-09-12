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

(defvar *vulkan-instance* nil)

(defmethod get-required-instance-extensions ((system-object t))
  #+linux
  (get-x11-required-instance-extensions)
  #+darwin
  (get-cocoa-required-instance-extensions)
  #+win32
  (get-win32-required-instance-extensions))

(defun get-vulkan-instance (&optional system-object)
  (if *vulkan-instance*
      *vulkan-instance*
      (setq *vulkan-instance*
	    (create-instance system-object))))

(defun enumerate-instance-layer-properties ()
  (with-foreign-object (p-property-count :int)
    (check-vk-result (vkEnumerateInstanceLayerProperties p-property-count +nullptr+))
    (let ((property-count (mem-aref p-property-count :int)))
      (with-foreign-object (p-properties '(:struct VkLayerProperties) property-count)
	(check-vk-result (vkEnumerateInstanceLayerProperties p-property-count p-properties))
	(remove-duplicates 
	 (loop for i from 0 below (mem-aref p-property-count :int)
	    collect (let ((p-property (mem-aptr p-properties '(:struct VkLayerProperties) i)))
		      (list (foreign-string-to-lisp (foreign-slot-pointer p-property '(:struct VkLayerProperties) '%vk::layerName))
			    (foreign-slot-value p-property '(:struct VkLayerProperties) '%vk::specVersion)
			    (foreign-slot-value p-property '(:struct VkLayerProperties) '%vk::implementationVersion)
			    (foreign-string-to-lisp (foreign-slot-pointer p-property '(:struct VkLayerProperties) '%vk::description)))))
	 :test #'equalp)))))
			 
(defun enumerate-instance-extension-properties (layer-name)
  (with-foreign-string (p-layer-name layer-name)
    (with-foreign-object (p-property-count :int)
      (check-vk-result (vkEnumerateInstanceExtensionProperties p-layer-name p-property-count +nullptr+))
      (let ((property-count (mem-aref p-property-count :int)))
	(with-foreign-object (p-properties '(:struct VkExtensionProperties) property-count)
	  (check-vk-result (vkEnumerateInstanceExtensionProperties p-layer-name p-property-count p-properties))
	  (remove-duplicates 
	   (loop for i from 0 below (mem-aref p-property-count :int)
	      append (let ((p-property (mem-aptr p-properties '(:struct VkExtensionProperties) i)))
			(list (foreign-string-to-lisp (foreign-slot-pointer p-property '(:struct VkExtensionProperties) '%vk::extensionName))
			      (foreign-slot-value p-property '(:struct VkExtensionProperties) '%vk::specVersion))))
	   :test #'equalp))))))

(defun available-layers ()
  (mapcar #'first (enumerate-instance-layer-properties)))

(defun available-extensions ()
  (let ((list (mapcar #'first (remove-duplicates (remove-if #'null (mapcar #'enumerate-instance-extension-properties (available-layers))) :test #'equalp))))
    #+darwin (list* "VK_KHR_portability_enumeration"
		    "VK_KHR_get_physical_device_properties2"
		    list)
    #-darwin list))

(defun destroy-vulkan-instance (instance)
  (when instance
    (when (debug-callback instance)
      (destroy-debug-report-callback (debug-callback instance)))
    (loop for device in (logical-devices instance)
       do (let ((command-pools (command-pools device)))
	    (loop for entry in command-pools
	       do (free-command-buffers (second entry))
		 (vkDestroyCommandPool (h device) (h (second entry)) (h (allocator device)))))
       finally (vkDestroyDevice (h device) (h (allocator device))))
    (vkDestroyInstance (h instance) (h (allocator instance)))
    (setq *vulkan-instance* nil)
    t))

(defun create-instance (system-object &key (title "CL-Vulkan Demo")
			  (application-name title)
			  (application-version 0)
			  (engine-name "")
			  (engine-version 0)
			  layer-names
			  extension-names
			  (api-version (api-version 1 3 250))
			  (allocator +null-allocator+)
			&allow-other-keys)

  #+(and sbcl darwin)(sb-int:set-floating-point-modes :traps nil)
  (let ((available-layers (available-layers))
	(available-extensions (available-extensions)))

    #+darwin(pushnew "VK_KHR_portability_enumeration" extension-names :test #'string=)
    #+darwin(pushnew "VK_KHR_get_physical_device_properties2" extension-names :test #'string=)
    
    (when (and (numberp *debug*) (> *debug* 1)
	       (find "VK_LAYER_LUNARG_api_dump" available-layers :test #'string=))
      (pushnew "VK_LAYER_LUNARG_api_dump" layer-names :test #'string=))
    
    (loop for layer in layer-names
       unless (find layer available-layers :test #'string=)
       do (error "layer ~S is not available" layer))
    
    (loop for ext in extension-names
       unless (find ext available-extensions :test #'string=)
       do (error "extension ~S is not available" ext)))

  (with-foreign-object (p-extensions-count :uint32)
    #+glfw
    (when (zerop (glfwInit))
      (error "GLFW failed to initialize."))
    
    (let* ((required-extensions (get-required-instance-extensions system-object))
	   (required-extension-count (length required-extensions))
	   (extension-count (+ (length extension-names) required-extension-count))
	   (layer-count (length layer-names)))

      (loop for extension in required-extensions
	   do (push extension extension-names))

      (with-foreign-objects
	  ((pp-enabled-extension-names-with-debug '(:pointer :char) (1+ extension-count))
	   (pp-enabled-extension-names '(:pointer :char) extension-count)
	   (pp-enabled-layer-names-with-validation '(:pointer :char) (1+ layer-count))
	   (pp-enabled-layer-names '(:pointer :char) layer-count))
	
	(unwind-protect
	     (progn
	       (loop for i from 0
		  for extension-string in extension-names
		  do
		    (setf
		     (mem-aref pp-enabled-extension-names-with-debug '(:pointer :char) i)
		     (foreign-string-alloc extension-string)
		     (mem-aref pp-enabled-extension-names '(:pointer :char) i)
		     (foreign-string-alloc extension-string))
		  finally
		    (setf
		     (mem-aref pp-enabled-extension-names-with-debug '(:pointer :char) i)
		     (foreign-string-alloc VK_EXT_DEBUG_REPORT_EXTENSION_NAME)))
	       
	       (loop for i from 0
		  for layer-string in layer-names
		  do
		    (setf
		     (mem-aref pp-enabled-layer-names-with-validation '(:pointer :char) i)
		     (foreign-string-alloc layer-string)
		     (mem-aref pp-enabled-layer-names '(:pointer :char) i)
		     (foreign-string-alloc layer-string))
		  finally
		    (setf
		     (mem-aref pp-enabled-layer-names-with-validation '(:pointer :char) i)
		     #-NVIDIA
		     (foreign-string-alloc "VK_LAYER_KHRONOS_validation")
		     #+NVIDIA
		     (foreign-string-alloc "VK_LAYER_LUNARG_standard_validation")))
	       
	       (with-vk-struct (p-application-info VkApplicationInfo)
		 (with-foreign-slots ((%vk::pApplicationName
				       %vk::applicationVersion
				       %vk::pEngineName
				       %vk::engineVersion
				       %vk::apiVersion)
				      p-application-info
				      (:struct VkApplicationInfo))
		   (with-foreign-strings ((p-application-name application-name)
					  (p-engine-name engine-name))
		     (setf %vk::pApplicationName p-application-name
			   %vk::applicationVersion application-version
			   %vk::pEngineName p-engine-name
			   %vk::engineVersion engine-version
			   %vk::apiVersion api-version)

		     (with-foreign-object (p-instance 'VkInstance)
		       (with-vk-struct (p-create-info VkInstanceCreateInfo)
			 ;; note: pNext takes pointers to structures (usually for callbacks)
			 ;; which should be implemented at some point.
			 (with-foreign-slots ((%vk::pApplicationInfo
					       %vk::enabledExtensionCount
					       %vk::ppEnabledExtensionNames
					       %vk::enabledLayerCount
					       %vk::ppEnabledLayerNames
					       %vk::flags)
					      p-create-info
					      (:struct VkInstanceCreateInfo))

			   (setf %vk::pApplicationInfo p-application-info)
			   #+darwin(setf %vk::flags %vk::VK_INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT)

			   (flet ((try-create-inst (&key debug validation)
				    (let ((1+extension-count (1+ extension-count))
					  (1+layer-count (1+ layer-count)))
				  
				      (if debug
					  (setf %vk::ppEnabledExtensionNames pp-enabled-extension-names-with-debug
						%vk::enabledExtensionCount 1+extension-count)
					  (setf %vk::ppEnabledExtensionNames	pp-enabled-extension-names
						%vk::enabledExtensionCount extension-count))
				  
				      (if validation
					  (setf %vk::ppEnabledLayerNames pp-enabled-layer-names-with-validation
						%vk::enabledLayerCount 1+layer-count)
					  (setf %vk::ppEnabledLayerNames pp-enabled-layer-names
						%vk::enabledLayerCount layer-count)))

				    (vkCreateInstance p-create-info (h allocator) p-instance)))

			     (let ((result)
				   (debug-report-present nil))
			       (block try
				 (if *debug*
				     (progn
				       (setq result (try-create-inst :validation t :debug t))
				       (if (eq result VK_ERROR_LAYER_NOT_PRESENT)
					   (progn (warn "Trying to create vulkan instance with VK_LAYER_KHRONOS_validation failed, falling back...")
						  (setq result (try-create-inst :debug t))
						  (if (eq result VK_ERROR_EXTENSION_NOT_PRESENT)
						      (progn
							(warn "Trying to create vulkan instance with VK_EXT_debug_report failed, falling back...")
							(check-vk-result (try-create-inst)))
						      (if (eq result VK_SUCCESS)
							  (setq debug-report-present t)
							  (check-vk-result result))))
					   (if (eq result VK_ERROR_EXTENSION_NOT_PRESENT)
					       (progn (warn "Trying to create vulkan instance with VK_EXT_debug_report failed, falling back...")
						      (setq result (try-create-inst :validation t))
						      (if (eq result VK_ERROR_LAYER_NOT_PRESENT)
							  (warn "Trying to create vulkan instance with VK_LAYER_LUNARG_STANDARD_VALIDATION failed, falling back...")
							  (if (eq result VK_SUCCESS)
							      (setq debug-report-present t)
							      (check-vk-result (try-create-inst)))))
					       (if (eq result VK_SUCCESS)
						   (setq debug-report-present t)
						   (check-vk-result result)))))
				     (check-vk-result (try-create-inst))))
			 
			       (setq *vulkan-instance*
				     (make-instance 'instance
						    :handle (mem-aref p-instance 'VkInstance)
						    :debug-report-present debug-report-present
						    :allocator allocator)))))))))))

	  (loop for i from 0 below extension-count
	     do (foreign-string-free (mem-aref pp-enabled-extension-names-with-debug '(:pointer :char) i)))
	  (loop for i from 0 below extension-count
	     do (foreign-string-free (mem-aref pp-enabled-extension-names '(:pointer :char) i)))
	  (loop for i from 0 below layer-count
	     do (foreign-string-free (mem-aref pp-enabled-layer-names-with-validation '(:pointer :char) i)))
	  (loop for i from 0 below layer-count
	     do (foreign-string-free (mem-aref pp-enabled-layer-names '(:pointer :char) i))))))))
