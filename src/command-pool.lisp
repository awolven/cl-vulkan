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

(defun create-command-pool (device queue-family-index &key (allocator +null-allocator+))
  (with-vk-struct (p-info VkCommandPoolCreateInfo)
    (with-foreign-slots ((%vk::flags %vk::queueFamilyIndex)
			 p-info (:struct VkCommandPoolCreateInfo))
      (setf %vk::flags VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
	    %vk::queueFamilyIndex queue-family-index)
      (with-foreign-object (p-command-pool 'VkCommandPool)
	(check-vk-result (vkCreateCommandPool (h device) p-info (h allocator) p-command-pool))
	(let ((command-pool
	       (make-instance 'command-pool :handle (mem-aref p-command-pool 'VkCommandPool)
			      :device device
			      :allocator allocator
			      :index queue-family-index)))
	  (push (list queue-family-index command-pool) (command-pools device))
	  command-pool)))))

(defun find-command-pool (device queue-family-index)
  (let ((entry (assoc queue-family-index (command-pools device))))
    (if entry
	(second entry) nil)))

(defun destroy-command-pool (command-pool)
  (with-slots (device allocator) command-pool
    (vkDestroyCommandPool (h device) (h command-pool) (h allocator))
    (setf (command-pools device) (remove-if #'(lambda (item)
						(pointer-eq (h (cadr item)) (h command-pool)))
					    (command-pools device))))
  (values))

(defun reset-command-pool (device command-pool)
  (check-vk-result
   (vkResetCommandPool (h device) (h command-pool) 0)))
