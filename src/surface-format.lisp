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

(defun get-surface-formats (gpu surface)
  (with-foreign-object (p-count :uint32)
    (check-vk-result (vkGetPhysicalDeviceSurfaceFormatsKHR (h gpu) (h surface) p-count +nullptr+))
    (let ((count (mem-aref p-count :uint32)))
      (let ((p-formats (foreign-alloc '(:struct VkSurfaceFormatKHR) :count count)))
	(check-vk-result (vkGetPhysicalDeviceSurfaceFormatsKHR (h gpu) (h surface) p-count p-formats))
	(loop for i from 0 below (mem-aref p-count :uint32)
	   collect (let ((p-format (mem-aptr p-formats '(:struct VkSurfaceFormatKHR) i)))
		     (make-instance 'surface-format
				    :format (foreign-slot-value p-format '(:struct VkSurfaceFormatKHR) '%vk::format)
				    :color-space (foreign-slot-value p-format '(:struct VkSurfaceFormatKHR) '%vk::colorSpace))))))))

(defun find-supported-depth-format (gpu &key (candidates
					      (list VK_FORMAT_D32_SFLOAT
						    VK_FORMAT_D32_SFLOAT_S8_UINT
						    Vk_FORMAT_D24_UNORM_S8_UINT))
					  
					  (tiling VK_IMAGE_TILING_OPTIMAL)
					  (features  VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT))
  (loop for format in candidates
     do (with-vk-struct (p-props VkFormatProperties)
	  (vkGetPhysicalDeviceFormatProperties (h gpu) format p-props)
	  (with-foreign-slots ((%vk::linearTilingFeatures
				%vk::optimalTilingFeatures)
			       p-props (:struct VkFormatProperties))

	    (when (and (eq tiling VK_IMAGE_TILING_LINEAR)
		       (eq (logand %vk::linearTilingFeatures features) features))
	      (return format))
	    (when (and (eq tiling VK_IMAGE_TILING_OPTIMAL)
		       (eq (logand %vk::optimalTilingFeatures features) features))
	      (return format))))
     finally (error "Failed to find supported format.")))

(defun find-supported-format (surface &key (requested-image-format VK_FORMAT_B8G8R8A8_UNORM)
					(requested-color-space VK_COLOR_SPACE_SRGB_NONLINEAR_KHR))
  (loop for format in (supported-formats surface)
     do (when (and (eq (surface-format-format format) requested-image-format)
		   (eq (surface-format-color-space format) requested-color-space))
	  (return format))
     finally (let ((first-format (first (supported-formats surface))))
	       (when (eq (surface-format-format first-format) VK_FORMAT_UNDEFINED)
		 (setf (surface-format-format first-format) requested-image-format
		       (surface-format-color-space first-format) requested-color-space))
	       (return first-format))))

(defun has-stencil-component-p (format)
  (or (eq format VK_FORMAT_D32_SFLOAT_S8_UINT)
      (eq format VK_FORMAT_D24_UNORM_S8_UINT)))
