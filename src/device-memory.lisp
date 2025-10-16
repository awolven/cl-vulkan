(in-package :vk)

(cffi:defcstruct VkPhysicalDeviceMemoryProperties2
  (sType VkStructureType)
  (pNext (:pointer :void))
  (memoryProperties (:struct VkPhysicalDeviceMemoryProperties)))

(defconstant VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2 1000059006)

(cffi:defcstruct VkPhysicalDeviceMemoryBudgetPropertiesEXT
  (sType VkStructureType)
  (pNext (:pointer :void))
  (heapBudget (:array VkDeviceSize 16))
  (heapUsage (:array VkDeviceSize 16)))

(defconstant VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT 1000237000)

(cffi:defcfun ("vkGetPhysicalDeviceMemoryProperties2" vkGetPhysicalDeviceMemoryProperties2) :void
  (physicalDevice VkPhysicalDevice)
  (pMemoryProperties (:pointer (:struct VkPhysicalDeviceMemoryProperties2))))

(defun get-heap-budgets-and-usage (gpu)
  (with-foreign-object (p-memory-properties2 '(:struct VkPhysicalDeviceMemoryProperties2))
    (setf (cffi:foreign-slot-value p-memory-properties2 '(:struct VkPhysicalDeviceMemoryProperties2) 'sType)
	  VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2)
    (with-foreign-object (p-memory-budget '(:struct VkPhysicalDeviceMemoryBudgetPropertiesEXT))
      (setf (cffi:foreign-slot-value p-memory-budget
				     '(:struct VkPhysicalDeviceMemoryBudgetPropertiesEXT) 'sType)
	    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT)
      (setf (cffi:foreign-slot-value p-memory-budget
				     '(:struct VkPhysicalDeviceMemoryBudgetPropertiesEXT) 'pNext)
	    +nullptr+)
      (setf (cffi:foreign-slot-value p-memory-properties2
				     '(:struct VkPhysicalDeviceMemoryProperties2) 'pNext)
	    p-memory-budget)
      (let ((p-heap-budget-array (cffi:foreign-slot-pointer
				  p-memory-properties2 '(:struct VkPhysicalDeviceMemoryBudgetPropertiesEXT)
				  'heapBudget))
	    (p-heap-usage-array (cffi:foreign-slot-pointer
				 p-memory-properties2 '(:struct VkPhysicalDeviceMemoryBudgetPropertiesEXT)
				 'heapUsage)))
	(vkGetPhysicalDeviceMemoryProperties2 (h gpu) p-memory-properties2)
	(loop for i from 0 below 16;;(get-memory-heap-count gpu)
	      collect (cons (cffi:mem-aref p-heap-budget-array 'VkDeviceSize i)
			    (cffi:mem-aref p-heap-usage-array 'VkDeviceSize i)))))))
			  
	  
