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

(defun find-memory-type (gpu type-filter properties)
  (with-vk-struct (p-mem-properties VkPhysicalDeviceMemoryProperties)
    (with-foreign-slots ((%vk::memoryTypeCount)
			 p-mem-properties
			 (:struct VkPhysicalDeviceMemoryProperties))
      
      (vkGetPhysicalDeviceMemoryProperties (h gpu) p-mem-properties)
      ;; todo: should be able to do this with cached values!
      (loop for i from 0 below %vk::memoryTypeCount
	 do (when (and (not (zerop (logand type-filter (ash 1 i))))
		       (not (zerop (logand
				    (foreign-slot-value
				     (mem-aptr (foreign-slot-pointer
						p-mem-properties
						'(:struct VkPhysicalDeviceMemoryProperties)
						'%vk::memoryTypes)
					       '(:struct VkMemoryType) i)
				     '(:struct VkMemoryType)
				     '%vk::propertyFlags)
				    properties))))
	      (return i))
	 finally (error "Could not find suitable memory type!")))))

(defmethod memory-type-device-local-p ((memory-type memory-type))
  (not (zerop (logand (memory-type-property-flags memory-type) VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT))))

(defmethod memory-type-host-visible-p ((memory-type memory-type))
  (not (zerop (logand (memory-type-property-flags memory-type) VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT))))

(defmethod memory-type-host-coherent-p ((memory-type memory-type))
  (not (zerop (logand (memory-type-property-flags memory-type) VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))))

(defmethod memory-type-host-cached-p ((memory-type memory-type))
  (not (zerop (logand (memory-type-property-flags memory-type) VK_MEMORY_PROPERTY_HOST_CACHED_BIT))))

(defmethod memory-type-lazily-allocated-p ((memory-type memory-type))
  (not (zerop (logand (memory-type-property-flags memory-type) VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT))))

(defconstant VK_MEMORY_PROPERTY_PROTECTED_BIT #x20)

(defmethod memory-type-protected-p ((memory-type memory-type))
  (not (zerop (logand (memory-type-property-flags memory-type) VK_MEMORY_PROPERTY_PROTECTED_BIT))))

(defun memory-type (gpu properties type-bits)
  (loop for memory-type in (first (memory-properties gpu)) for i from 0
     do (when (and (eq (memory-type-property-flags memory-type) properties)
		   (not (zerop (logand type-bits (ash i 1)))))
	  (return i))
     finally (return nil)))
