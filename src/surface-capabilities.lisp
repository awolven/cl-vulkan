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

(defmethod capabilities-current-extent-width ((cap surface-capabilities))
  (extent-2D-width (slot-value cap 'current-extent)))

(defmethod capabilities-current-extent-height ((cap surface-capabilities))
  (extent-2D-height (slot-value cap 'current-extent)))

(defun get-physical-device-surface-capabilities-khr (gpu surface)
  (with-foreign-object (p-surface-capabilities '(:struct VkSurfaceCapabilitiesKHR))
    (check-vk-result (vkGetPhysicalDeviceSurfaceCapabilitiesKHR (h gpu) (h surface) p-surface-capabilities))
    (with-foreign-slots ((%vk::minImageCount
			  %vk::maxImageCount
			  %vk::maxImageArrayLayers
			  %vk::supportedTransforms
			  %vk::currentTransform
			  %vk::supportedCompositeAlpha
			  %vk::supportedUsageFlags)
			 p-surface-capabilities
			 (:struct VkSurfaceCapabilitiesKHR))
      (make-instance 'surface-capabilities
		     :min-image-count %vk::minImageCount
		     :max-image-count %vk::maxImageCount
		     :current-extent (let ((p-extent
					    (foreign-slot-pointer p-surface-capabilities
								  '(:struct VkSurfaceCapabilitiesKHR)
								  '%vk::currentExtent)))
				       (make-instance 'extent-2D
						      :width (foreign-slot-value p-extent
										 '(:struct VkExtent2D)
										 '%vk::width)
						      :height (foreign-slot-value p-extent
										 '(:struct VkExtent2D)
										 '%vk::height)))
		     :min-image-extent (let ((p-extent
					       (foreign-slot-pointer p-surface-capabilities
								     '(:struct VkSurfaceCapabilitiesKHR)
								     '%vk::minImageExtent)))
					  (make-instance 'extent-2D
							 :width (foreign-slot-value p-extent
										    '(:struct VkExtent2D)
										    '%vk::width)
							 :height (foreign-slot-value p-extent
										     '(:struct VkExtent2D)
										     '%vk::height)))
		     :max-image-extent (let ((p-extent
					      (foreign-slot-pointer p-surface-capabilities
								    '(:struct VkSurfaceCapabilitiesKHR)
								    '%vk::maxImageExtent)))
					 (make-instance 'extent-2D
							:width (foreign-slot-value p-extent
										   '(:struct VkExtent2D)
										   '%vk::width)
							:height (foreign-slot-value p-extent
										    '(:struct VkExtent2D)
										    '%vk::height)))
		     :max-image-array-layers %vk::maxImageArrayLayers
		     :supported-transforms %vk::supportedTransforms
		     :current-transform %vk::currentTransform
		     :supported-composite-alpha %vk::supportedCompositeAlpha
		     :supported-usage-flags %vk::supportedUsageFlags))))
