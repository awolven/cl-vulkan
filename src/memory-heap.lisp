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

(defun memory-heap-device-local-p (memory-heap)
  (logtest (memory-heap-flags memory-heap) VK_MEMORY_HEAP_DEVICE_LOCAL_BIT))

(defconstant VK_MEMORY_HEAP_MULTI_INSTANCE_BIT 2)

(defun memory-heap-multi-instance-p (memory-heap)
  (logtest (memory-heap-flags memory-heap) VK_MEMORY_HEAP_MULTI_INSTANCE_BIT))

(defun get-memory-heap-count (gpu)
  (with-foreign-object (p-memory-properties '(:struct VkPhysicalDeviceMemoryProperties))
    (vkGetPhysicalDeviceMemoryProperties (h gpu) p-memory-properties)
    (foreign-slot-value p-memory-properties '(:struct VkPhysicalDeviceMemoryProperties) '%vk::memoryHeapCount)))

(defun %get-memory-heaps (gpu-handle)
  (with-foreign-object (p-memory-properties '(:struct VkPhysicalDeviceMemoryProperties))
    (vkGetPhysicalDeviceMemoryProperties gpu-handle p-memory-properties)
    (loop for i from 0 below (foreign-slot-value p-memory-properties '(:struct VkPhysicalDeviceMemoryProperties) '%vk::memoryHeapCount)
       collect (let ((p-memory-heap  (mem-aptr (foreign-slot-pointer p-memory-properties '(:struct VkPhysicalDeviceMemoryProperties) '%vk::memoryHeaps)
					       '(:struct VkMemoryHeap)
					       i)))
		 (make-instance 'memory-heap
				:flags (foreign-slot-value p-memory-heap '(:struct VkMemoryHeap) '%vk::flags)
				:size (foreign-slot-value p-memory-heap '(:struct VkMemoryHeap) '%vk::size))))))
				

(defun get-memory-heap-size (gpu heap-index)
  (with-foreign-object (p-memory-properties '(:struct VkPhysicalDeviceMemoryProperties))
    (vkGetPhysicalDeviceMemoryProperties (h gpu) p-memory-properties)
    (when (< heap-index (foreign-slot-value p-memory-properties '(:struct VkPhysicalDeviceMemoryProperties) '%vk::memoryHeapCount))
      (let ((p-memory-heap (mem-aptr (foreign-slot-pointer p-memory-properties '(:struct VkPhysicalDeviceMemoryProperties) '%vk::memoryHeaps)
				     '(:struct VkMemoryHeap)
				     heap-index)))
	(foreign-slot-value p-memory-heap '(:struct VkMemoryHeap) '%vk::size)))))

(defun get-memory-heap-flags (gpu heap-index)
  (with-foreign-object (p-memory-properties '(:struct VkPhysicalDeviceMemoryProperties))
    (vkGetPhysicalDeviceMemoryProperties (h gpu) p-memory-properties)
    (when (< heap-index (foreign-slot-value p-memory-properties '(:struct VkPhysicalDeviceMemoryProperties) '%vk::memoryHeapCount))
      (let ((p-memory-heap (mem-aptr (foreign-slot-pointer p-memory-properties '(:struct VkPhysicalDeviceMemoryProperties) '%vk::memoryHeaps)
				     '(:struct VkMemoryHeap)
				     heap-index)))
	(foreign-slot-value p-memory-heap '(:struct VkMemoryHeap) '%vk::flags)))))


