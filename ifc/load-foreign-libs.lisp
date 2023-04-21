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

(in-package :cl-user)

(defparameter *home-dir*
  (namestring (user-homedir-pathname)))

(defparameter *cl-vulkan-dir*
  (namestring (asdf/system:system-relative-pathname :cl-vulkan "")))

#+(and sbcl darwin)
(let ((vulkan-sdk-path (concatenate 'string *home-dir* "/VulkanSDK/1.3.231.1/macOS")))
  
  (sb-posix:setenv "VULKAN_SDK" vulkan-sdk-path 0)
  (sb-posix:setenv "DYLD_LIBRARY_PATH" (concatenate 'string (sb-posix:getenv "DYLD_LIBRARY_PATH") ":" vulkan-sdk-path "/lib") 0)
  (sb-posix:setenv "VK_ADD_LAYER_PATH" (concatenate 'string vulkan-sdk-path "/share/vulkan/explicit_layer.d") 0)
  (sb-posix:setenv "VK_ICD_FILENAMES" (concatenate 'string vulkan-sdk-path "/share/vulkan/icd.d/MoltenVK_icd.json") 0)
  (sb-posix:setenv "VK_DRIVER_FILES" (concatenate 'string vulkan-sdk-path "/share/vulkan/icd.d/MoltenVK_icd.json") 0)
  
;;  (sb-posix:setenv "VULKAN_FRAMEWORK_PATH" (concatenate 'string vulkan-sdk-path "/Frameworks") 0)
  ;;  (sb-posix:setenv "DYLD_FRAMEWORK_PATH" (concatenate 'string vulkan-sdk-path "/Frameworks") 0)
  )

#+(and ccl darwin)
(let ((vulkan-sdk-path (concatenate 'string *home-dir* "/VulkanSDK/1.3.231.1/macOS")))
  
  (setf (uiop/os:getenv "VULKAN_SDK") vulkan-sdk-path)
  (setf (uiop/os:getenv "DYLD_LIBRARY_PATH") (concatenate 'string (uiop/os:getenv "DYLD_LIBRARY_PATH") ":" vulkan-sdk-path "/lib"))
  (setf (uiop/os:getenv "VK_ADD_LAYER_PATH") (concatenate 'string vulkan-sdk-path "/share/vulkan/explicit_layer.d"))
  (setf (uiop/os:getenv "VK_ICD_FILENAMES") (concatenate 'string vulkan-sdk-path "/share/vulkan/icd.d/MoltenVK_icd.json"))
  (setf (uiop/os:getenv "VK_DRIVER_FILES") (concatenate 'string vulkan-sdk-path "/share/vulkan/icd.d/MoltenVK_icd.json"))
  )

#+swiftshader
(sb-posix:setenv "VK_ICD_FILENAMES"
		 (concatenate 'string *home-dir* "/swiftshader-build1/Linux/vk_swiftshader_icd.json") 0)

#+glfw
(cffi:define-foreign-library glfw3
  (:darwin "libglfw.3.dylib")
  (:windows "glfw3.dll")
  (:linux "libglfw.so.3"))

(cffi:define-foreign-library vulkan-loader
  (:linux "libvulkan.so.1")
  (:darwin "libvulkan.1.dylib")
  (:windows "vulkan-1.dll"))

#+glfw
(cffi:use-foreign-library glfw3)

(cffi:use-foreign-library vulkan-loader)
