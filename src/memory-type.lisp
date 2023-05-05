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



(defun %memory-type-property-flags (p-memory-type)
  (foreign-slot-value p-memory-type '(:struct VkMemoryType) '%vk::propertyFlags))

(defun %memory-type-heap-index (p-memory-type)
  (foreign-slot-value p-memory-type '(:struct VkMemoryType) '%vk::heapIndex))

(defun %get-memory-types (gpu-handle)
  (with-foreign-object (p-memory-properties '(:struct VkPhysicalDeviceMemoryProperties))
    (vkGetPhysicalDeviceMemoryProperties gpu-handle p-memory-properties)
    (loop for i from 0 below (foreign-slot-value p-memory-properties '(:struct VkPhysicalDeviceMemoryProperties) '%vk::memoryTypeCount)
       collect (let ((p-memory-type (mem-aptr (foreign-slot-pointer p-memory-properties '(:struct VkPhysicalDeviceMemoryProperties) '%vk::memoryTypes)
					      '(:struct VkMemoryType)
					      i)))
		 (make-instance 'memory-type
				:property-flags (%memory-type-property-flags p-memory-type)
				:heap-index (%memory-type-heap-index p-memory-type))))))

(defun memory-type-device-local-p (memory-type)
  (logtest (memory-type-property-flags memory-type) VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT))

(defun memory-type-host-visible-p (memory-type)
  (logtest (memory-type-property-flags memory-type) VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT))

(defun memory-type-host-coherent-p (memory-type)
  (logtest (memory-type-property-flags memory-type) VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))

(defun memory-type-host-cached-p (memory-type)
  (logtest (memory-type-property-flags memory-type) VK_MEMORY_PROPERTY_HOST_CACHED_BIT))

(defun memory-type-lazily-allocated-p (memory-type)
  (logtest (memory-type-property-flags memory-type) VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT))

(defconstant VK_MEMORY_PROPERTY_PROTECTED_BIT #x20)

(defun memory-type-protected-p (memory-type)
  (logtest (memory-type-property-flags memory-type) VK_MEMORY_PROPERTY_PROTECTED_BIT))

(defun find-memory-type (gpu type-bits properties)
  (loop for memory-type in (memory-types gpu)
     for i from 0 
     when (and (logtest type-bits (ash 1 i))
	       (= (logand (memory-type-property-flags memory-type) properties) properties))
     do (return i)
     finally (error "Could not find suitable memory type!")))

