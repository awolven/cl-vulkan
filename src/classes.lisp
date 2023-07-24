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

#+SBCL
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-concurrency))

#+glfw
(defclass handle-mixin ()
  ((handle :accessor h :initarg :handle)))

(defmethod print-object ((object handle-mixin) stream)
  (format stream "#<~A ~A>" (class-name (class-of object))
	  (if (slot-boundp object 'handle) (h object) "NO HANDLE")))

(defclass allocator-mixin ()
  ((allocator :reader allocator :initarg :allocator)))

(defclass logical-device-mixin (allocator-mixin)
  ((device :initarg :device :reader device)))  

(defclass pointer-mixin ()
  ((pointer :accessor p :initarg :pointer)))

(defclass allocation-callbacks (handle-mixin)
  ((handle :initform +nullptr+)))

(defclass pipeline-cache (handle-mixin)
  ())

(defclass instance (handle-mixin allocator-mixin)
  ((logical-devices :initform () :accessor logical-devices)
   (debug-report-present :initarg :debug-report-present :reader debug-report-present?)
   (debug-callback :accessor debug-callback :initform nil)))

(defclass base-device (handle-mixin allocator-mixin)
  ((pipeline-cache :accessor pipeline-cache)
   (command-pools :accessor command-pools :initform nil)
   (descriptor-pools :accessor descriptor-pools :initform nil)
   (queues :initform nil :accessor device-queues)
   (stock-render-passess :initform (make-hash-table) :accessor stock-render-passes)))

(defclass sgpu-device (base-device)
  ((physical-device :initarg :physical-device :reader physical-device)))

(defclass mgpu-device (base-device)
  ((parent-physical-devices
    :reader parent-physical-devices
    :initform (make-array 3 :adjustable t :fill-pointer 0))))

(defclass swapchain (handle-mixin logical-device-mixin)
  ((surface-format :initarg :surface-format :reader surface-format)
   (number-of-images :accessor number-of-images)
   (images :accessor images)
   (color-image-views :accessor color-image-views)
   (depth-image-view :accessor depth-image-view)
   (depth-image :accessor depth-image)
   (fb-width  :initarg :width  :reader fb-width)
   (fb-height :initarg :height :reader fb-height)
   (framebuffers :accessor framebuffers)
   (render-pass :accessor render-pass :initarg :render-pass)
   (frame-resources :accessor frame-resources)
   (current-frame :accessor current-frame :initform 0)))

(defclass frame-resources ()
  ((fence :reader fence :initarg :fence)
   (present-complete-semaphore
    :reader present-complete-semaphore
    :initarg :present-complete-semaphore)
   (render-complete-semaphore
    :reader render-complete-semaphore
    :initarg :render-complete-semaphore)
   (command-buffer :reader frame-command-buffer :initarg :command-buffer)
   (command-pool :reader frame-command-pool :initarg :command-pool)))

(defclass vulkan-window-mixin (logical-device-mixin)
  ((swapchain :initform nil :accessor swapchain)
   (render-pass :initform nil :accessor render-pass)
   (desired-format :initform VK_FORMAT_B8G8R8A8_UNORM :accessor window-desired-format
		   :initarg :format)
   (desired-color-space :initform VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
			:accessor window-desired-color-space
			:initarg :color-space)
   (application :accessor application :initarg :app)
   (surface :initform nil :accessor render-surface)
   (command-pool :accessor command-pool)
   (clear-value
    :initform (make-array 4 :element-type 'single-float
			  :initial-contents (list 0.45f0 0.55f0 0.60f0 1.0f0))
    :accessor clear-value)
   (recreate-swapchain? :initform nil :accessor recreate-swapchain?)
   (new-width :initform nil :accessor new-width)
   (new-height :initform nil :accessor new-height)
   
   (frame-data :initform nil :accessor window-frame-data)
   (image-index :initform 0)
   (current-frame :initform 0)))

#+noglfw
(defclass vulkan-helper-window (clui::handle-mixin)
  ((surface :initform nil :accessor render-surface)
   (surface-format :initform nil :accessor surface-format)
   (render-pass :initform nil :accessor render-pass)))

(defclass vulkan-window (vulkan-window-mixin)
  ())

(defclass debug-report-callback (handle-mixin)
  ((callback-name :initarg :callback-name
		  :reader callback-name)
   (allocator :initarg :allocator :reader allocator)))

(defclass surface-format ()
  ((format :initarg :format :accessor surface-format-format)
   (color-space :initarg :color-space :accessor surface-format-color-space)))

(defclass device-surface-format (surface-format)
  ((render-pass :accessor device-surface-format-render-pass)))

(defclass surface (handle-mixin logical-device-mixin)
  ((paired-gpu :initform nil :accessor paired-gpu)
   (window :accessor window :initarg :window)
   (queue-family-index :accessor queue-family-index)
   (capabilities :accessor capabilities)
   (supported-formats :initform nil :accessor supported-formats)
   (presentation-modes :accessor presentation-modes)))

(defclass physical-device-features ()
  ((robust-buffer-access
    :reader has-robust-buffer-access-p
    :initarg :robust-buffer-access)
   (full-draw-index-uint32
    :reader has-full-draw-index-uint32-p
    :initarg :full-draw-index-uint32)
   (image-cube-array
    :reader has-image-cube-array-p
    :initarg :image-cube-array)
   (independent-blend
    :reader has-independent-blend-p
    :initarg :independent-blend)
   (geometry-shader
    :reader has-geometry-shader-p
    :initarg :geometry-shader)
   (tessellation-shader
    :reader has-tessellation-shader-p
    :initarg :tessellation-shader)
   (sample-rate-shading
    :reader has-sample-rate-shading-p
    :initarg :sample-rate-shading)
   (dual-src-blend
    :reader has-dual-src-blend-p
    :initarg :dual-src-blend)
   (logic-op
    :reader has-logic-op-p
    :initarg :logic-op)
   (multi-draw-indirect
    :reader has-multi-draw-indirect-p
    :initarg :multi-draw-indirect)
   (draw-indirect-first-instance
    :reader has-draw-indirect-first-instance-p
    :initarg :draw-indirect-first-instance)
   (depth-clamp
    :reader has-depth-clamp-p
    :initarg :depth-clamp)
   (depth-bias-clamp
    :reader has-depth-bias-clamp-p
    :initarg :depth-bias-clamp)
   (fill-mode-non-solid
    :reader has-fill-mode-non-solid-p
    :initarg :fill-mode-non-solid)
   (depth-bounds
    :reader has-depth-bounds-p
    :initarg :depth-bounds)
   (wide-lines
    :reader has-wide-lines-p
    :initarg :wide-lines)
   (large-points
    :reader has-large-points-p
    :initarg :large-points)
   (alpha-to-one
    :reader has-alpha-to-one-p
    :initarg :alpha-to-one)
   (multi-viewport
    :reader has-multi-viewport-p
    :initarg :multi-viewport)
   (sampler-anisotropy
    :reader has-sampler-anisotropy-p
    :initarg :sampler-anisotropy)
   (texture-compression-etc2
    :reader has-texture-compression-etc2-p
    :initarg :texture-compression-etc2)
   (texture-compression-astc-ldr
    :reader has-texture-compression-astc-ldr-p
    :initarg :texture-compression-astc-ldr)
   (texture-compression-bc
    :reader has-texture-compression-bc-p
    :initarg :texture-compression-bc)
   (occlusion-query-precise
    :reader has-occlusion-query-precise-p
    :initarg :occlusion-query-precise)
   (pipeline-statistics-query
    :reader has-pipeline-statistics-query-p
    :initarg :pipeline-statistics-query)
   (vertex-pipeline-stores-and-atomics
    :reader has-vertex-pipeline-stores-and-atomics-p
    :initarg :vertex-pipeline-stores-and-atomics)
   (fragment-stores-and-atomics
    :reader has-fragment-stores-and-atomics-p
    :initarg :fragment-stores-and-atomics)
   (shader-tessellation-and-geometry-point-size
    :reader has-shader-tessellation-and-geometry-point-size-p
    :initarg :shader-tessellation-and-geometry-point-size)
   (shader-image-gather-extended
    :reader has-shader-image-gather-extended-p
    :initarg :shader-image-gather-extended)
   (shader-storage-image-extended-formats
    :reader has-shader-storage-image-extended-formats-p
    :initarg :shader-storage-image-extended-formats)
   (shader-storage-image-multisample
    :reader has-shader-storage-image-multisample-p
    :initarg :shader-storage-image-multisample)
   (shader-storage-image-read-without-format
    :reader has-shader-storage-image-read-without-format-p
    :initarg :shader-storage-image-read-without-format)
   (shader-storage-image-write-without-format
    :reader has-shader-storage-image-write-without-format-p
    :initarg :shader-storage-image-write-without-format)
   (shader-uniform-buffer-array-dynamic-indexing
    :reader has-shader-uniform-buffer-array-dynamic-indexing-p
    :initarg :shader-uniform-buffer-array-dynamic-indexing)
   (shader-sampled-image-array-dynamic-indexing
    :reader has-shader-sampled-image-array-dynamic-indexing-p
    :initarg :shader-sampled-image-array-dynamic-indexing)
   (shader-storage-buffer-array-dynamic-indexing
    :reader has-shader-storage-buffer-array-dynamic-indexing-p
    :initarg :shader-storage-buffer-array-dynamic-indexing)
   (shader-storage-image-array-dynamic-indexing
    :reader has-shader-storage-image-array-dynamic-indexing-p
    :initarg :shader-storage-image-array-dynamic-indexing)
   (shader-clip-distance
    :reader has-shader-clip-distance-p
    :initarg :shader-clip-distance)
   (shader-cull-distance
    :reader has-shader-cull-distance-p
    :initarg :shader-cull-distance)
   (shader-float64
    :reader has-shader-float64-p
    :initarg :shader-float64)
   (shader-int64
    :reader has-shader-int64-p
    :initarg :shader-int64)
   (shader-int16
    :reader has-shader-int16-p
    :initarg :shader-int16)
   (shader-resource-residency
    :reader has-shader-resource-residency-p
    :initarg :shader-resource-residency)
   (shader-resource-min-lod
    :reader has-shader-resource-min-lod-p
    :initarg :shader-resource-min-lod)
   (sparse-binding
    :reader has-sparse-binding-p
    :initarg :sparse-binding)
   (sparse-residency-buffer
    :reader has-sparse-residency-buffer-p
    :initarg :sparse-residency-buffer)
   (sparse-residency-image-2D
    :reader has-sparse-residency-image-2D-p
    :initarg :sparse-residency-image-2D)
   (sparse-residency-image-3D
    :reader has-sparse-residency-image-3D-p
    :initarg :sparse-residency-image-3D)
   (sparse-residency2-samples
    :reader has-sparse-residency2-samples-p
    :initarg :sparse-residency2-samples)
   (sparse-residency4-samples
    :reader has-sparse-residency4-samples-p
    :initarg :sparse-residency4-samples)
   (sparse-residency8-samples
    :reader has-sparse-residency8-samples-p
    :initarg :sparse-residency8-samples)
   (sparse-residency16-samples
    :reader has-sparse-residency16-samples-p
    :initarg :sparse-residency16-samples)
   (sparse-residency-aliased
    :reader has-sparse-residency-aliased-p
    :initarg :sparse-residency-aliased)
   (variable-multisample-rate
    :reader has-variable-multisample-rate-p
    :initarg :variable-multisample-rate)
   (inherited-queries
    :reader has-inherited-queries-p
    :initarg :inherited-queries)

   ;; features Vulkan 1.2
   (sampler-mirror-clamp-to-edge
    :reader has-sampler-mirror-clamp-to-edge-p
    :initarg :sampler-mirror-clamp-to-edge)
   (draw-indirect-count
    :reader has-draw-indirect-count-p
    :initarg :draw-indirect-count)
   (storage-buffer-8-bit-access
    :reader has-storage-buffer-8-bit-access-p
    :initarg :storage-buffer-8-bit-access)
   (uniform-and-storage-buffer-8-bit-access
    :reader has-uniform-and-storage-buffer-8-bit-access-p
    :initarg :uniform-and-storage-buffer-8-bit-access)
   (storage-push-constant-8
    :reader has-storage-push-constant-8-p
    :initarg :storage-push-constant-8)
   (shader-buffer-int64-atomics
    :reader has-shader-buffer-int64-atomics-p
    :initarg :shader-buffer-int64-atomics)
   (shader-shared-int64-atomics
    :reader has-shader-shared-int64-atomics
    :initarg :shader-shared-int64-atomics)
   (shader-float16
    :reader has-shader-float16-p
    :initarg :shader-float16)
   (shader-int8
    :reader has-shader-int8-p
    :initarg :shader-int8)
   (descriptor-indexing
    :reader has-descriptor-indexing-p
    :initarg :descriptor-indexing)
   (shader-input-attachment-array-dynamic-indexing
    :reader has-shader-input-attachment-array-dynamic-indexing-p
    :initarg :shader-input-attachment-array-dynamic-indexing)
   (shader-uniform-texel-buffer-array-dynamic-indexing
    :reader has-shader-uniform-texel-buffer-array-dynamic-indexing-p
    :initarg :shader-uniform-texel-buffer-array-dynamic-indexing)
   (shader-storage-texel-buffer-array-dynamic-indexing
    :reader has-shader-storage-texel-buffer-array-dynamic-indexing-p
    :initarg :shader-storage-texel-buffer-array-dynamic-indexing)
   (shader-uniform-buffer-array-non-uniform-indexing
    :reader has-shader-uniform-buffer-array-non-uniform-indexing-p
    :initarg :shader-uniform-buffer-array-non-uniform-indexing)
   (shader-sampled-image-array-non-uniform-indexing
    :reader has-shader-sampled-image-array-non-uniform-indexing-p
    :initarg :shader-sampled-image-array-non-uniform-indexing)
   (shader-storage-buffer-array-non-uniform-indexing
    :reader has-shader-storage-buffer-array-non-uniform-indexing-p
    :initarg :shader-storage-buffer-array-non-uniform-indexing)
   (shader-storage-image-array-non-uniform-indexing
    :reader has-shader-storage-image-array-non-uniform-indexing-p
    :initarg :shader-storage-image-array-non-uniform-indexing)
   (shader-input-attachment-array-non-uniform-indexing
    :reader has-shader-input-attachment-array-non-uniform-indexing-p
    :initarg :shader-input-attachment-array-non-uniform-indexing)
   (shader-uniform-texel-buffer-array-non-uniform-indexing
    :reader has-shader-uniform-texel-buffer-array-non-uniform-indexing-p
    :initarg :shader-uniform-texel-buffer-array-non-uniform-indexing)
   (shader-storage-texel-buffer-array-non-uniform-indexing
    :reader has-shader-storage-texel-buffer-array-non-uniform-indexing-p
    :initarg :shader-storage-texel-buffer-array-non-uniform-indexing)
   (descriptor-binding-uniform-buffer-update-after-bind
    :reader has-descriptor-binding-uniform-buffer-update-after-bind-p
    :initarg :descriptor-binding-uniform-buffer-update-after-bind)
   (descriptor-binding-sampled-image-update-after-bind
    :reader has-descriptor-binding-sampled-image-update-after-bind-p
    :initarg :descriptor-binding-sampled-image-update-after-bind)
   (descriptor-binding-storage-image-update-after-bind
    :reader has-descriptor-binding-storage-image-update-after-bind-p
    :initarg :descriptor-binding-storage-image-update-after-bind)
   (descriptor-binding-storage-buffer-update-after-bind
    :reader has-descriptor-binding-storage-buffer-update-after-bind-p
    :initarg :descriptor-binding-storage-buffer-update-after-bind)
   (descriptor-binding-uniform-texel-buffer-update-after-bind
    :reader has-descriptor-binding-uniform-texel-buffer-update-after-bind-p
    :initarg :descriptor-binding-uniform-texel-buffer-update-after-bind)
   (descriptor-binding-storage-texel-buffer-update-after-bind
    :reader has-descriptor-binding-storage-texel-buffer-update-after-bind-p
    :initarg :descriptor-binding-storage-texel-buffer-update-after-bind)
   (descriptor-binding-update-unused-while-pending
    :reader has-descriptor-binding-update-unused-while-pending-p
    :initarg :descriptor-binding-update-unused-while-pending)
   (descriptor-binding-partially-bound
    :reader has-descriptor-binding-partially-bound-p
    :initarg :descriptor-binding-partially-bound)
   (descriptor-binding-variable-descriptor-count
    :reader has-descriptor-binding-variable-descriptor-count
    :initarg :descriptor-binding-variable-descriptor-count)
   (runtime-descriptor-array
    :reader has-runtime-descriptor-array-p
    :initarg :runtime-descriptor-array)
   (sampler-filter-minmax
    :reader has-sampler-filter-minmax-p
    :initarg :sampler-filter-minmax)
   (scalar-block-layout
    :reader has-scalar-block-layout-p
    :initarg :scalar-block-layout)
   (imageless-framebuffer
    :reader has-imageless-framebuffer-p
    :initarg :imageless-framebuffer)
   (uniform-buffer-standard-layout
    :reader has-uniform-buffer-standard-layout-p
    :initarg :uniform-buffer-standard-layout)
   (shader-subgroup-extended-types
    :reader has-shader-subgroup-extended-types-p
    :initarg :shader-subgroup-extended-types)
   (separate-depth-stencil-layouts
    :reader has-separate-depth-stencil-layouts-p
    :initarg :separate-depth-stencil-layouts)
   (host-query-reset
    :reader has-host-query-reset-p
    :initarg :host-query-reset)
   (timeline-semaphore
    :reader has-timeline-semaphore-p
    :initarg :timeline-semaphore)
   (buffer-device-address
    :reader has-buffer-device-address-p
    :initarg :buffer-device-address)
   (buffer-device-address-capture-replay
    :reader has-buffer-device-address-capture-replay-p
    :initarg :buffer-device-address-capture-replay)
   (buffer-device-address-multi-device
    :reader has-buffer-device-address-multi-device-p
    :initarg :buffer-device-address-multi-device)
   (vulkan-memory-model
    :reader has-vulkan-memory-model-p
    :initarg :vulkan-memory-model)
   (vulkan-memory-model-device-scope
    :reader has-vulkan-memory-model-device-scope-p
    :initarg :vulkan-memory-model-device-scope)
   (vulkan-memory-model-availability-visibility-chains
    :reader has-vulkan-memory-model-availability-visibility-chains-p
    :initarg :vulkan-memory-model-availability-visibility-chains)
   (shader-output-viewport-index
    :reader has-shader-output-viewport-index-p
    :initarg :shader-output-viewport-index)
   (shader-output-layer
    :reader has-shader-output-layer-p
    :initarg :shader-output-layer)
   (subgroup-broadcast-dynamic-id
    :reader has-subgroup-broadcast-dynamic-id-p
    :initarg :subgroup-broadcast-dynamic-id)))
   

(defclass physical-device-limits ()
  ((max-image-dimension-1D
    :reader max-image-dimension-1D
    :initarg :max-image-dimension-1D)
   (max-image-dimension-2D
    :reader max-image-dimension-2D
    :initarg :max-image-dimension-2D)
   (max-image-dimension-3D
    :reader max-image-dimension-3D
    :initarg :max-image-dimension-3D)
   (max-image-dimension-cube
    :reader max-image-dimension-cube
    :initarg :max-image-dimension-cube)
   (max-image-array-layers
    :reader max-image-array-layers
    :initarg :max-image-array-layers)
   (max-texel-buffer-elements
    :reader max-texel-buffer-elements
    :initarg :max-texel-buffer-elements)
   (max-uniform-buffer-range
    :reader max-uniform-buffer-range
    :initarg :max-uniform-buffer-range)
   (max-storage-buffer-range
    :reader max-storage-buffer-range
    :initarg :max-storage-buffer-range)
   (max-push-constants-size
    :reader max-push-constants-size
    :initarg :max-push-constants-size)
   (max-memory-allocation-count
    :reader max-memory-allocation-count
    :initarg :max-memory-allocation-count)
   (max-sampler-allocation-count
    :reader max-sampler-allocation-count
    :initarg :max-sampler-allocation-count)
   (buffer-image-granularity
    :reader buffer-image-granularity
    :initarg :buffer-image-granularity)
   (sparse-address-space-size
    :reader sparse-address-space-size
    :initarg :sparse-address-space-size)
   (max-bound-descriptor-sets
    :reader max-bound-descriptor-sets
    :initarg :max-bound-descriptor-sets)
   (max-per-stage-descriptor-samplers
    :reader max-per-stage-descriptor-samplers
    :initarg :max-per-stage-descriptor-samplers)
   (max-per-stage-descriptor-uniform-buffers
    :reader max-per-stage-descriptor-uniform-buffers
    :initarg :max-per-stage-descriptor-uniform-buffers)
   (max-per-stage-descriptor-storage-buffers
    :reader max-per-stage-descriptor-storage-buffers
    :initarg :max-per-stage-descriptor-storage-buffers)
   (max-per-stage-descriptor-sampled-images
    :reader max-per-stage-descriptor-sampled-images
    :initarg :max-per-stage-descriptor-sampled-images)
   (max-per-stage-descriptor-storage-images
    :reader max-per-stage-descriptor-storage-images
    :initarg :max-per-stage-descriptor-storage-images)
   (max-per-stage-descriptor-input-attachments
    :reader max-per-stage-descriptor-input-attachments
    :initarg :max-per-stage-descriptor-input-attachments)
   (max-per-stage-resources
    :reader max-per-stage-resources
    :initarg :max-per-stage-resources)
   (max-descriptor-set-samplers
    :reader max-descriptor-set-samplers
    :initarg :max-descriptor-set-samplers)
   (max-descriptor-set-uniform-buffers
    :reader max-descriptor-set-uniform-buffers
    :initarg :max-descriptor-set-uniform-buffers)
   (max-descriptor-set-uniform-buffers-dynamic
    :reader max-descriptor-set-uniform-buffers-dynamic
    :initarg :max-descriptor-set-uniform-buffers-dynamic)
   (max-descriptor-set-storage-buffers
    :reader max-descriptor-set-storage-buffers
    :initarg :max-descriptor-set-storage-buffers)
   (max-descriptor-set-storage-buffers-dynamic
    :reader max-descriptor-set-storage-buffers-dynamic
    :initarg :max-descriptor-set-storage-buffers-dynamic)
   (max-descriptor-set-sampled-images
    :reader max-descriptor-set-sampled-images
    :initarg :max-descriptor-set-sampled-images)
   (max-descriptor-set-storage-images
    :reader max-descriptor-set-storage-images
    :initarg :max-descriptor-set-storage-images)
   (max-descriptor-set-input-attachments
    :reader max-descriptor-set-input-attachments
    :initarg :max-descriptor-set-input-attachments)
   (max-vertex-input-attributes
    :reader max-vertex-input-attributes
    :initarg :max-vertex-input-attributes)
   (max-vertex-input-bindings
    :reader max-vertex-input-bindings
    :initarg :max-vertex-input-bindings)
   (max-vertex-input-attribute-offset
    :reader max-vertex-input-attribute-offset
    :initarg :max-vertex-input-attribute-offset)
   (max-vertex-input-binding-stride
    :reader max-vertex-input-binding-stride
    :initarg :max-vertex-input-binding-stride)
   (max-vertex-output-components
    :reader max-vertex-output-components
    :initarg :max-vertex-output-components)
   (max-tessellation-generation-level
    :reader max-tessellation-generation-level
    :initarg :max-tessellation-generation-level)
   (max-tessellation-patch-size
    :reader max-tessellation-patch-size
    :initarg :max-tessellation-patch-size)
   (max-tessellation-control-per-vertex-input-components
    :reader max-tessellation-control-per-vertex-input-components
    :initarg :max-tessellation-control-per-vertex-input-components)
   (max-tessellation-control-per-vertex-output-components
    :reader max-tessellation-control-per-vertex-output-components
    :initarg :max-tessellation-control-per-vertex-output-components)
   (max-tessellation-control-per-patch-output-components
    :reader max-tessellation-control-per-patch-output-components
    :initarg :max-tessellation-control-per-patch-output-components)
   (max-tessellation-control-total-output-components
    :reader max-tessellation-control-total-output-components
    :initarg :max-tessellation-control-total-output-components)
   (max-tessellation-evaluation-input-components
    :reader max-tessellation-evaluation-input-components
    :initarg :max-tessellation-evaluation-input-components)
   (max-tessellation-evaluation-output-components
    :reader max-tessellation-evaluation-output-components
    :initarg :max-tessellation-evaluation-output-components)
   (max-geometry-shader-invocations
    :reader max-geometry-shader-invocations
    :initarg :max-geometry-shader-invocations)
   (max-geometry-input-components
    :reader max-geometry-input-components
    :initarg :max-geometry-input-components)
   (max-geometry-output-components
    :reader max-geometry-output-components
    :initarg :max-geometry-output-components)
   (max-geometry-output-vertices
    :reader max-geometry-output-vertices
    :initarg :max-geometry-output-vertices)
   (max-geometry-total-output-components
    :reader max-geometry-total-output-components
    :initarg :max-geometry-total-output-components)
   (max-fragment-input-components
    :reader max-fragment-input-components
    :initarg :max-fragment-input-components)
   (max-fragment-output-attachments
    :reader max-fragment-output-attachments
    :initarg :max-fragment-output-attachments)
   (max-fragment-dual-src-attachments
    :reader max-fragment-dual-src-attachments
    :initarg :max-fragment-dual-src-attachments)
   (max-fragment-combined-output-resources
    :reader max-fragment-combined-output-resources
    :initarg :max-fragment-combined-output-resources)
   (max-compute-shared-memory-size
    :reader max-compute-shared-memory-size
    :initarg :max-compute-shared-memory-size)
   (max-compute-work-group-count
    :reader max-compute-work-group-count
    :initarg :max-compute-work-group-count)
   (max-compute-work-group-invocations
    :reader max-compute-work-group-invocations
    :initarg :max-compute-work-group-invocations)
   (max-compute-work-group-size
    :reader max-compute-work-group-size
    :initarg :max-compute-work-group-size)
   (sub-pixel-precision-bits
    :reader sub-pixel-precision-bits
    :initarg :sub-pixel-precision-bits)
   (sub-texel-precision-bits
    :reader sub-texel-precision-bits
    :initarg :sub-texel-precision-bits)
   (mipmap-precision-bits
    :reader mipmap-precision-bits
    :initarg :mipmap-precision-bits)
   (max-draw-indexed-index-value
    :reader max-draw-indexed-index-value
    :initarg :max-draw-indexed-index-value)
   (max-draw-indirect-count
    :reader max-draw-indirect-count
    :initarg :max-draw-indirect-count)
   (max-sampler-lod-bias
    :reader max-sampler-lod-bias
    :initarg :max-sampler-lod-bias)
   (max-sampler-anisotropy
    :reader max-sampler-anisotropy
    :initarg :max-sampler-anisotropy)
   (max-viewports
    :reader max-viewports
    :initarg :max-viewports)
   (max-viewport-dimensions
    :reader max-viewport-dimensions
    :initarg :max-viewport-dimensions)
   (viewport-bounds-range
    :reader viewport-bounds-range
    :initarg :viewport-bounds-range)
   (viewport-sub-pixel-bits
    :reader viewport-sub-pixel-bits
    :initarg :viewport-sub-pixel-bits)
   (min-memory-map-alignment
    :reader min-memory-map-alignment
    :initarg :min-memory-map-alignment)
   (min-texel-buffer-offset-alignment
    :reader min-texel-buffer-offset-alignment
    :initarg :min-texel-buffer-offset-alignment)
   (min-uniform-buffer-offset-alignment
    :reader min-uniform-buffer-offset-alignment
    :initarg :min-uniform-buffer-offset-alignment)
   (min-storage-buffer-offset-alignment
    :reader min-storage-buffer-offset-alignment
    :initarg :min-storage-buffer-offset-alignment)
   (min-texel-offset
    :reader min-texel-offset
    :initarg :min-texel-offset)
   (max-texel-offset
    :reader max-texel-offset
    :initarg :max-texel-offset)
   (min-texel-gather-offset
    :reader min-texel-gather-offset
    :initarg :min-texel-gather-offset)
   (max-texel-gather-offset
    :reader max-texel-gather-offset
    :initarg :max-texel-gather-offset)
   (min-interpolation-offset
    :reader min-interpolation-offset
    :initarg :min-interpolation-offset)
   (max-interpolation-offset
    :reader max-interpolation-offset
    :initarg :max-interpolation-offset)
   (sub-pixel-interpolation-offset-bits
    :reader sub-pixel-interpolation-offset-bits
    :initarg :sub-pixel-interpolation-offset-bits)
   (max-framebuffer-width
    :reader max-framebuffer-width
    :initarg :max-framebuffer-width)
   (max-framebuffer-height
    :reader max-framebuffer-height
    :initarg :max-framebuffer-height)
   (max-framebuffer-layers
    :reader max-framebuffer-layers
    :initarg :max-framebuffer-layers)
   (framebuffer-color-sample-counts
    :reader framebuffer-color-sample-counts
    :initarg :framebuffer-color-sample-counts)
   (framebuffer-depth-sample-counts
    :reader framebuffer-depth-sample-counts
    :initarg :framebuffer-depth-sample-counts)
   (framebuffer-stencil-sample-counts
    :reader framebuffer-stencil-sample-counts
    :initarg :framebuffer-stencil-sample-counts)
   (framebuffer-no-attachments-sample-counts
    :reader framebuffer-no-attachments-sample-counts
    :initarg :framebuffer-no-attachments-sample-counts)
   (max-color-attachments
    :reader max-color-attachments
    :initarg :max-color-attachments)
   (sampled-image-color-sample-counts
    :reader sampled-image-color-sample-counts
    :initarg :sampled-image-color-sample-counts)
   (sampled-image-integer-sample-counts
    :reader sampled-image-integer-sample-counts
    :initarg :sampled-image-integer-sample-counts)
   (sampled-image-depth-sample-counts
    :reader sampled-image-depth-sample-counts
    :initarg :sampled-image-depth-sample-counts)
   (sampled-image-stencil-sample-counts
    :reader sampled-image-stencil-sample-counts
    :initarg :sampled-image-stencil-sample-counts)
   (storage-image-sample-counts
    :reader storage-image-sample-counts
    :initarg :storage-image-sample-counts)
   (max-sample-mask-words
    :reader max-sample-mask-words
    :initarg :max-sample-mask-words)
   (timestamp-compute-and-graphics
    :reader timestamp-compute-and-graphics
    :initarg :timestamp-compute-and-graphics)
   (timestamp-period
    :reader timestamp-period
    :initarg :timestamp-period)
   (max-clip-distances
    :reader max-clip-distances
    :initarg :max-clip-distances)
   (max-cull-distances
    :reader max-cull-distances
    :initarg :max-cull-distances)
   (max-combined-clip-and-cull-distances
    :reader max-combined-clip-and-cull-distances
    :initarg :max-combined-clip-and-cull-distances)
   (discrete-queue-priorities
    :reader discrete-queue-priorities
    :initarg :discrete-queue-priorities)
   (point-size-range
    :reader point-size-range
    :initarg :point-size-range)
   (line-width-range
    :reader line-width-range
    :initarg :line-width-range)
   (point-size-granularity
    :reader point-size-granularity
    :initarg :point-size-granularity)
   (line-width-granularity
    :reader line-width-granularity
    :initarg :line-width-granularity)
   (strict-lines
    :reader strict-lines
    :initarg :strict-lines)
   (standard-sample-locations
    :reader standard-sample-locations
    :initarg :standard-sample-locations)
   (optimal-buffer-copy-offset-alignment
    :reader optimal-buffer-copy-offset-alignment
    :initarg :optimal-buffer-copy-offset-alignment)
   (optimal-buffer-copy-row-pitch-alignment
    :reader optimal-buffer-copy-row-pitch-alignment
    :initarg :optimal-buffer-copy-row-pitch-alignment)
   (non-coherent-atom-size
    :reader non-coherent-atom-size
    :initarg :non-coherent-atom-size)))

(defclass physical-device (handle-mixin physical-device-features physical-device-limits)
  ((index :initarg :index)
   (pipeline-cache-uuid :initarg :pipeline-cache-uuid)
   (device-name :initarg :device-name :reader device-name)
   (device-type :initarg :device-type :reader device-type)
   (device-id :initarg :device-id :reader device-id)
   (vendor-id :initarg :vendor-id :reader vendor-id)
   (driver-version :initarg :driver-version :reader driver-version)
   (api-version :initarg :api-version :reader physical-device-api-version)
   (device-group-index)
   (device-group-device-index)
   (queue-families :accessor queue-families)
   (memory-heaps :accessor memory-heaps :initarg :memory-heaps)
   (memory-types :accessor memory-types :initarg :memory-types)))

(defmethod print-object ((gpu physical-device) stream)
  (format stream "#<GPU ~A>" (device-name gpu)))

(defclass queue-family ()
  ((queue-flags :initarg :queue-flags :reader queue-flags)
   (queue-count :initarg :queue-count :reader queue-count)
   (timestamp-valid-bits :initarg :timestamp-valid-bits :reader timestamp-valid-bits)
   (min-image-transfer-granularity :initarg :min-image-transfer-granularity)))

(defclass extent-3D ()
  ((width :initarg :width :reader extent-3D-width)
   (height :initarg :height :reader extent-3D-height)   
   (depth :initarg :depth :reader extent-3D-depth)))

(defclass memory-type ()
  ((property-flags :initarg :property-flags :reader memory-type-property-flags)
   (heap-index :initarg :heap-index :reader memory-type-heap-index)))

(defclass memory-heap ()
  ((flags :initarg :flags :reader memory-heap-flags)
   (size :initarg :size :reader memory-heap-size)))

(defclass surface-capabilities ()
  ((min-image-count :initarg :min-image-count :reader min-image-count)
   (max-image-count :initarg :max-image-count :reader max-image-count)
   (current-extent :initarg :current-extent)
   (min-image-extent :initarg :min-image-extent)
   (max-image-extent :initarg :max-image-extent)
   (max-image-array-layers :initarg :max-image-array-layers :reader max-image-array-layers)
   (supported-transforms :initarg :supported-transforms :reader supported-transforms)
   (current-transform :initarg :current-transform :reader current-transform)
   (supported-composite-alpha :initarg :supported-composite-alpha :reader supported-composite-alpha)
   (supported-usage-flags :initarg :supported-usage-flags :reader supported-usage-flags)))

(defclass extent-2D ()
  ((width :initarg :width :reader extent-2D-width)
   (height :initarg :height :reader extent-2D-height)))

(defclass queue (handle-mixin logical-device-mixin)
  ((family-index :initarg :family-index :reader queue-family-index)
   (index :initarg :index :reader queue-index)))

(defclass dedicated-queue (queue) ())

(defclass multipurpose-queue (queue) ())

(defclass allocated-memory-mixin ()
  ((size :initarg :size :reader size)
   (memory :initform nil :accessor allocated-memory :initarg :memory)))

(defclass image (handle-mixin logical-device-mixin allocated-memory-mixin)
  ())

(defclass depth-image (image)
  ())

(defclass image-view (handle-mixin logical-device-mixin)
  ((image :initarg :image :reader image)))

(defclass render-pass (handle-mixin logical-device-mixin)
  ()) 

(defclass attachment ()
  ((name  :initarg :name :reader attachment-name)
   (format :initarg :format :reader attachment-format)
   (samples :initarg :samples :reader samples)
   (load-op :initarg :load-op :reader load-op)
   (store-op :initarg :store-op :reader store-op)
   (stencil-load-op :initarg :stencil-load-op :reader stencil-load-op)
   (stencil-store-op :initarg :stencil-store-op :reader stencil-store-op)
   (initial-layout :initarg :initial-layout :reader initial-layout)
   (final-layout :initarg :final-layout :reader final-layout)
   (reference-layout :initarg :reference-layout :reader reference-layout)))
  

(defclass color-attachment (attachment)
  ((samples :initform VK_SAMPLE_COUNT_1_BIT)
   (load-op  :initform VK_ATTACHMENT_LOAD_OP_CLEAR)
   (store-op :initform VK_ATTACHMENT_STORE_OP_STORE)
   (stencil-load-op :initform VK_ATTACHMENT_LOAD_OP_DONT_CARE)
   (stencil-store-op :initform VK_ATTACHMENT_STORE_OP_DONT_CARE)
   (initial-layout :initform VK_IMAGE_LAYOUT_UNDEFINED)
   (final-layout :initform VK_IMAGE_LAYOUT_PRESENT_SRC_KHR)
   (reference-layout :initform VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL)))

(defclass depth-attachment (attachment)
  ((samples :initform VK_SAMPLE_COUNT_1_BIT)
   (load-op :initform VK_ATTACHMENT_LOAD_OP_CLEAR)
   (store-op :initform VK_ATTACHMENT_STORE_OP_DONT_CARE)
   (stencil-load-op :initform VK_ATTACHMENT_LOAD_OP_DONT_CARE)
   (stencil-store-op :initform VK_ATTACHMENT_STORE_OP_DONT_CARE)
   (initial-layout :initform VK_IMAGE_LAYOUT_UNDEFINED)
   (final-layout :initform VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL)
   (reference-layout :initform VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL)))


(defclass subpass ()
  ((name :initarg :name :reader subpass-name)
   (pipeline-bind-point :initarg :pipeline-bind-point :reader pipeline-bind-point :initform VK_PIPELINE_BIND_POINT_GRAPHICS)
   (color-attachments :initarg :color-attachments :reader color-attachments)
   (depth-attachments :initarg :depth-attachments :reader depth-attachments)
   (dependencies :initarg :dependencies :initform nil :reader dependencies)))

(defclass subpass-dependency ()
  ((src-subpass :initarg :src-subpass :initform 0 :reader src-subpass)
   (dst-subpass :initarg :dst-subpass :initform 0 :reader dst-subpass)
   (src-access-mask :initarg :src-access-mask :initform 0 :reader src-access-mask)
   (src-stage-mask :initarg :src-stage-mask :initform 0 :reader src-stage-mask)
   (dst-access-mask :initarg :dst-access-mask :initform 0 :reader dst-access-mask)
   (dst-stage-mask :initarg :dst-stage-mask :initform 0 :reader dst-stage-mask)))

(defclass descriptor-set-layout-binding ()
  ((binding :initarg :binding :initform 0 :reader binding)
   (descriptor-type :initarg :type :initform nil :reader descriptor-type)
   (descriptor-count :initarg :count :initform 1 :reader descriptor-count)
   (stage-flags :initarg :flags :reader stage-flags)
   (immutable-samplers :initform nil :initarg :samplers :reader immutable-samplers)))

(defclass uniform-buffer-for-vertex-shader-dsl-binding (descriptor-set-layout-binding)
  ((descriptor-type :initform VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER)
   (stage-flags :initform VK_SHADER_STAGE_VERTEX_BIT)))

(defclass uniform-buffer-for-fragment-shader-dsl-binding (descriptor-set-layout-binding)
  ((descriptor-type :initform VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER)
   (stage-flags :initform VK_SHADER_STAGE_FRAGMENT_BIT)))

(defclass storage-buffer-for-fragment-shader-dsl-binding (descriptor-set-layout-binding)
  ((descriptor-type :initform VK_DESCRIPTOR_TYPE_STORAGE_BUFFER)
   (stage-flags :initform VK_SHADER_STAGE_FRAGMENT_BIT)))

(defclass uniform-buffer-for-geometry-shader-dsl-binding (descriptor-set-layout-binding)
  ((binding :initform 1)
   (descriptor-type :initform VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER)
   (stage-flags :initform VK_SHADER_STAGE_GEOMETRY_BIT)))

(defclass sample-uniform-buffer-for-compute-shader-dsl-binding (descriptor-set-layout-binding)
  ((binding :initform 0)
   (descriptor-type :initform VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER)
   (stage-flags :initform VK_SHADER_STAGE_COMPUTE_BIT)))

(defclass sample-input-storage-buffer-for-compute-shader-dsl-binding (descriptor-set-layout-binding)
  ((binding :initform 1)
   (descriptor-type :initform VK_DESCRIPTOR_TYPE_STORAGE_BUFFER)
   (stage-flags :initform VK_SHADER_STAGE_COMPUTE_BIT)))

(defclass sample-output-storage-buffer-for-compute-shader-dsl-binding (descriptor-set-layout-binding)
  ((binding :initform 2)
   (descriptor-type :initform VK_DESCRIPTOR_TYPE_STORAGE_BUFFER)
   (stage-flags :initform VK_SHADER_STAGE_COMPUTE_BIT)))

(defclass descriptor-set-layout (handle-mixin logical-device-mixin)
  ())

(defclass null-descriptor-set-layout (descriptor-set-layout)
  ((handle :initform VK_NULL_HANDLE)))

(defclass push-constant-range ()
  ((stage-flags :initarg :stage-flags :reader push-constant-range-stage-flags)
   (offset :initarg :offset :reader push-constant-range-offset)
   (size :initarg :size :reader push-constant-range-size)))

(defclass pipeline-layout (handle-mixin logical-device-mixin)
  ((dsls :reader descriptor-set-layouts :initform (make-array 10 :adjustable t :fill-pointer 0))
   (push-constant-ranges :reader push-constant-ranges :initarg :push-constant-ranges)))

(defclass shader-module (handle-mixin logical-device-mixin)
  ())

(defclass pipeline (handle-mixin logical-device-mixin)
  ())

(defclass compute-pipeline (pipeline)
  ())

(defclass graphics-pipeline (pipeline)
  ((vertex-shader :accessor vertex-shader :initarg :vertex-shader)
   (geometry-shader :accessor geometry-shader :initarg :geometry-shader)
   (fragment-shader :accessor fragment-shader :initarg :fragment-shader)))

(defclass vertex-input-attribute-description ()
  ((location :initarg :location :reader location)
   (binding :initarg :binding :initform 0 :reader binding)
   (format :initarg :format :initform VK_FORMAT_R32G32B32_SFLOAT :reader desc-format)
   (offset :initarg :offset :reader offset)))

(defclass compute-pipeline-create-info ()
  ((flags :initform 0 :initarg :flags)
   (stage :initarg :stage :initform nil)
   (layout :initarg :layout :initform nil)
   (base-pipeline :initarg :base-pipeline :initform +nullptr+)
   (base-pipeline-index :initarg :base-pipeline-index :initform 0)))

(defclass shader-stage-create-info ()
  ((stage :initform #x00000020 :initarg :stage)
   (module :initarg :module :initform nil)
   (name :initarg :name :initform "main")
   (specialization-info :initarg :specialization-info :initform nil)))

(defclass command-pool (handle-mixin logical-device-mixin)
  ((queue-family-index :initarg :index :reader queue-family-index)
   (command-buffers :accessor command-buffers :initform (make-array 4 :adjustable t :fill-pointer 0))))

(defclass framebuffer (handle-mixin logical-device-mixin)
  ())

(defclass buffer (handle-mixin logical-device-mixin allocated-memory-mixin)
  ())

(defclass vertex-buffer (buffer)
  ())

(defclass index-buffer (buffer)
  ())

(defclass allocated-memory (handle-mixin logical-device-mixin)
  ((alignment :initarg :alignment :reader alignment)
   (size :initarg :size :reader allocated-memory-size)
   (memory-type :initarg :properties :reader allocated-memory-memory-type)))


(defclass uniform-buffer (buffer)
  ())

(defclass storage-buffer (buffer)
  ())

(defclass descriptor-pool (handle-mixin logical-device-mixin)
  ())

(defclass descriptor-set (handle-mixin)
  ((device :reader :device :initarg :device)
   (descriptor-pool :reader descriptor-pool :initarg :descriptor-pool)))

(defclass descriptor-buffer-info ()
  ((descriptor-type :reader descriptor-type :initform nil)
   (buffer :reader buffer :initarg :buffer)
   (offset :reader offset :initarg :offset :initform 0)
   (range :reader range :initarg :range)))

(defclass descriptor-uniform-buffer-info (descriptor-buffer-info)
  ((descriptor-type :reader descriptor-type :initform VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER)))

(defclass descriptor-storage-buffer-info (descriptor-buffer-info)
  ((descriptor-type :reader descriptor-type :initform VK_DESCRIPTOR_TYPE_STORAGE_BUFFER)))

(defclass descriptor-image-info ()
  ((vk::descriptor-type :reader vk::descriptor-type
		    :initform VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER)
   (image-layout :reader descriptor-image-info-image-layout :initarg :image-layout
		 :initform VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)
   (image-view :reader descriptor-image-info-image-view :initarg :image-view)
   (sampler :reader descriptor-image-info-sampler :initarg :sampler)))

(defclass command-buffer (handle-mixin logical-device-mixin)
  ((command-pool :initarg :command-pool :reader command-pool)))

(defclass fence (handle-mixin logical-device-mixin)
  ())

(defclass semaphore (handle-mixin logical-device-mixin)
  ())

(defclass sampler (handle-mixin logical-device-mixin)
  ())

(defstruct draw-indexed-cmd
  (index-count)
  (first-index)
  (vertex-offset)
  (color)
  (draw-list))

(defparameter +null-allocator+ (make-instance 'allocation-callbacks :handle +nullptr+))

(defparameter +null-pipeline-cache+ (make-instance 'pipeline-cache :handle VK_NULL_HANDLE))

(defparameter +null-swapchain+ (make-instance 'swapchain :handle +nullptr+))

(defparameter +null-descriptor-set-layout+ (make-instance 'null-descriptor-set-layout))

#+glfw
(defclass glfw-application-mixin ()
  ((shift-key-down? :accessor shift-key-down? :initform nil)
   (ctrl-key-down? :accessor ctrl-key-down? :initform nil)
   (meta-key-down? :accessor meta-key-down? :initform nil)
   (super-key-down? :accessor super-key-down? :initform nil)
   (mouse-down? :accessor mouse-down? :initform (vector nil nil nil))
   (current-cursor-pos :accessor current-cursor-pos :initform nil)
   (previous-cursor-pos :accessor previous-cursor-pos :initform nil)
   (mouse-delta :accessor mouse-delta :initform nil)))

(defclass vulkan-application-mixin ()
  ((application-name :accessor application-name :initform "CL-Vulkan Demo" :initarg :name)))

(defclass vulkan-enabled-display-mixin ()
  ((default-logical-device :accessor default-logical-device)
   (system-gpus :accessor system-gpus :initform nil)
   (pipeline-cache :initform +null-pipeline-cache+ :initarg :pipeline-cache :reader pipeline-cache)
   (default-descriptor-pool :accessor default-descriptor-pool)
   (allocation-callbacks :initform +null-allocator+ :initarg :allocator :reader allocator)
   (window-registry :initform nil :accessor window-registry)
   (main-window :accessor main-window)
   (memory-pool
    :accessor memory-pool
    :initform nil)))

(defclass vulkan-module ()
  ((application :reader application :initarg :application)))
