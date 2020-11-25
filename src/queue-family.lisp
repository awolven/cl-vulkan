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

(defmethod graphics-queue-family-p ((queue-family queue-family))
  (not (zerop (logand (queue-flags queue-family) VK_QUEUE_GRAPHICS_BIT))))

(defmethod compute-queue-family-p ((queue-family queue-family))
  (not (zerop (logand (queue-flags queue-family) VK_QUEUE_COMPUTE_BIT))))

(defmethod transfer-queue-family-p ((queue-family queue-family))
  (not (zerop (logand (queue-flags queue-family) VK_QUEUE_TRANSFER_BIT))))

(defmethod sparse-binding-queue-family-p ((queue-family queue-family))
  (not (zerop (logand (queue-flags queue-family) VK_QUEUE_SPARSE_BINDING_BIT))))

(defmethod min-image-transfer-granularity-width ((queue-family queue-family))
  (extent-3D-width (slot-value queue-family 'min-image-transfer-granularity)))

(defmethod min-image-transfer-granularity-height ((queue-family queue-family))
  (extent-3D-height (slot-value queue-family 'min-image-transfer-granularity)))

(defmethod min-image-transfer-granularity-depth ((queue-family queue-family))
  (extent-3D-depth (slot-value queue-family 'min-image-transfer-granularity)))
