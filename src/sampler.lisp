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

(defun create-sampler (device &key (allocator +null-allocator+))
  (with-vk-struct (p-info VkSamplerCreateInfo)
    (with-foreign-slots ((%vk::magFilter
			  %vk::minFilter
			  %vk::mipmapMode
			  %vk::addressModeU
			  %vk::addressModeV
			  %vk::addressModeW
			  %vk::minLod
			  %vk::maxLod
			  %vk::maxAnisotropy)
			 p-info
			 (:struct VkSamplerCreateInfo))
      (setf %vk::magFilter VK_FILTER_LINEAR
	    %vk::minFilter VK_FILTER_LINEAR
	    %vk::mipmapMode VK_SAMPLER_MIPMAP_MODE_LINEAR
	    %vk::addressModeU VK_SAMPLER_ADDRESS_MODE_REPEAT
	    %vk::addressModeV VK_SAMPLER_ADDRESS_MODE_REPEAT
	    %vk::addressModeW VK_SAMPLER_ADDRESS_MODE_REPEAT
	    %vk::minLod -1000.0f0
	    %vk::maxLod 1000.0f0
	    %vk::maxAnisotropy 1.0f0)
      (with-foreign-object (p-sampler 'VkSampler)
	(check-vk-result (vkCreateSampler (h device) p-info (h allocator) p-sampler))
	(make-instance 'sampler
		       :handle (mem-aref p-sampler 'VkSampler)
		       :device device)))))
