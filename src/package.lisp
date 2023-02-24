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

(in-package :cl-user)

(cl:defpackage :vk
  (:use :cl :cffi :cffi-sys :%vk #+glfw :$glfw)

  #+noglfw(:import-from :clui #:h #:handle-mixin #:handle)
  
  (:export #:vulkan-enabled-display-mixin
	   #:VK_WHOLE_SIZE
	   #:allocation-callbacks
	   #:pipeline-cache
	   #:*vulkan-instance*
	   #:vulkan-window-mixin
	   #:vulkan-window
	   #:destroy-vulkan-instance
	   #:free-command-buffers
	   #:sgpu-device
	   #:mgpu-device
	   #:swapchain
	   #:+nullptr+
	   #:+null-allocator+
	   #:+null-pipeline-cache+
	   #:+null-swapchain+
	   #:+null-descriptor-set-layout+
	   #:vulkan-application-mixin
	   #:frame-resources
	   #:debug-report-callback
	   #:surface-format
	   #:surface
	   #:physical-device-features
	   #:physical-device-limits
	   #:physical-device
	   #:queue-family
	   #:extent-3D
	   #:memory-type
	   #:memory-heap
	   #:surface-capabilities
	   #:extent-2D
	   #:dedicated-queue
	   #:multipurpose-queue
	   ;;#:image
	   #:depth-image
	   #:image-view
	   #:render-pass
	   #:color-attachment
	   #:depth-attachment
	   #:subpass
	   #:descriptor-set-layout-binding
	   #:uniform-buffer-for-vertex-shader-dsl-binding
	   #:storage-buffer-for-fragment-shader-dsl-binding
	   #:uniform-buffer-for-geometry-shader-dsl-binding
	   #:sample-uniform-buffer-for-compute-shader-dsl-binding
	   #:sample-input-storage-buffer-for-compute-shader-dsl-binding
	   #:descriptor-set-layout
	   #:null-descriptor-set-layout
	   #:push-constant-range
	   #:pipeline-layout
	   #:shader-module
	   #:pipeline
	   #:compute-pipeline
	   #:graphics-pipeline
	   #:vertex-input-attribute-description
	   #:compute-pipeline-create-info
	   #:shader-stage-create-info
	   #:command-pool
	   #:framebuffer
	   #:vertex-buffer
	   #:index-buffer
	   #:allocated-memory
	   #:uniform-buffer
	   #:descriptor-pool
	   #:descriptor-set
	   #:descriptor-buffer-info
	   #:descriptor-uniform-buffer-info
	   #:descriptor-storage-buffer-info
	   #:command-buffer
	   #:sampler
	   #:draw-index-cmd
	   #:acquire-memory-sized
	   #:release-memory

	   #:with-fences
	   #:create-buffer-1
	   #:destroy-buffer
	   #:allocate-buffer-memory
	   #:bind-buffer-memory
	   #:copy-buffer
	   #:create-empty-buffer
	   #:create-buffer
	   #:create-vertex-buffer
	   #:create-index-buffer
	   #:create-uniform-buffer
	   #:copy-uniform-buffer-memory
	   #:create-command-buffer
	   #:create-command-buffer-1
	   #:free-command-buffers
	   #:free-command-buffer
	   #:begin-command-buffer
	   #:cmd-set-viewport
	   #:cmd-set-scissor
	   #:cmd-bind-pipeline
	   #:cmd-bind-vertex-buffers
	   #:cmd-bind-descriptor-sets
	   #:cmd-bind-index-buffer
	   #:cmd-draw-indexed
	   #:create-command-pool
	   #:find-command-pool
	   #:destroy-command-pool
	   #:reset-command-pool
	   #:create-compute-pipeline
	   #:debug-report-function
	   #:create-debug-report-callback
	   #:destroy-debug-report-callback
	   #:create-descriptor-pool
	   #:create-descriptor-pool-1
	   #:create-descriptor-set-layout
	   #:destroy-descriptor-set-layout
	   #:allocate-descriptor-set
	   #:create-descriptor-set
	   #:set-framebuffer-size-callback
	   #:create-framebuffer
	   #:setup-framebuffers
	   #:resize-framebuffer
	   #:destroy-framebuffers
	   #:destroy-framebuffer
	   #:create-graphics-pipeline
	   #:destroy-pipeline
	   #:get-swapchain-images-khr
	   #:create-image
	   #:create-depth-image
	   #:destroy-image
	   #:transition-image-layout
	   #:create-image-view
	   #:create-depth-image-view
	   #:create-image-views
	   #:destroy-image-view
	   #:create-logical-device
	   #:begin-single-time-commands
	   #:end-single-time-commands
	   #:device-wait-idle
	   #:memory-heap-device-local-p
	   #:memory-heap-multi-instance-p
	   #:find-memory-type
	   #:memory-type-device-local-p
	   #:memory-type-host-visible-p
	   #:memory-type-host-coherent-p
	   #:memory-type-host-cached-p
	   #:memory-type-lazily-allocated-p
	   #:memory-type-protected-p
	   #:get-queue-family-index-with-dedicated-compute-support
	   #:get-any-queue-family-index-with-compute-support
	   #:get-queue-family-index-with-dedicated-transfer-support
	   #:get-any-queue-family-index-with-transfer-support
	   #:get-physical-device-memory-properties
	   #:get-physical-device-queue-family-properties
	   #:enumerate-physical-devices
	   #:create-pipeline-layout
	   #:destroy-pipeline-layout
	   #:get-present-modes
	   #:graphics-queue-family-p
	   #:compute-queue-family-p
	   #:transfer-queue-family-p
	   #:sparse-binding-queue-family-p
	   #:min-image-transfer-granularity-width
	   #:min-image-transfer-granularity-height
	   #:min-image-transfer-granularity-depth
	   #:get-device-queue
	   #:find-queue
	   #:compute-queue
	   #:queue-wait-idle
	   #:queue-submit1
	   #:queue-submit
	   #:create-render-pass
	   #:destroy-render-pass
	   #:create-sampler
	   #:create-shader-module-from-file
	   #:read-shader-file
	   #:create-shader-module
	   #:destroy-shader-module
	   #:capabilities-current-extent-width
	   #:capabilities-current-extent-height
	   #:get-physical-device-surface-capabilities-khr
	   #:get-surface-formats
	   #:find-supported-depth-format
	   #:find-supported-format
	   #:has-stencil-component-p
	   #:get-queue-family-index-with-wsi-support
	   #:supports-presentation-mode-p
	   #:create-window-surface
	   #:initialize-window-surface
	   #:pick-graphics-gpu
	   #:create-frame-resources
	   #:destroy-frame-resources
	   #:create-swapchain
	   #:initialize-swapchain
	   #:destroy-swapchain-resources
	   #:recreate-swapchain
	   #:destroy-swapchain
	   #:frame-begin
	   #:frame-end
	   #:frame-present
	   #:setup-vulkan
	   #:available-layers
	   #:available-extensions
	   #:create-instance
	   #:error-callback-function
	   #:set-window-close-callback
	   ;;#:find-window
	   #:default-application-class-for-window
	   #:default-window-class-for-application
	   #:create-vulkan-window
	   #:default-logical-device
	   #:main-window
	   #:queue-family-index
	   #:render-surface
	   #:command-buffers
	   #:end-command-buffer
	   #:main
	   #:h
	   #:recreate-swapchain?
	   #:frame-command-buffer
	   #:clear-value
	   #:number-of-images
	   #:default-descriptor-pool
	   #:frame-resource
	   #:check-vk-result
	   #:allocator
	   #:mmap-buffer
	   #:window-frame-data
	   #:window-registry
	   #:vulkan-module
	   #:vulkan-instance
	   #:current-frame
	   #:image-index
	   
	   #:with-vertex-input-binding-description
	   #:with-pipeline-vertex-input-state-create-info
	   #:with-pipeline-input-assembly-state-create-info
	   #:with-viewport-structure
	   #:with-scissor-structure
	   #:with-pipeline-viewport-state-create-info
	   #:with-pipeline-rasterization-state-create-info
	   #:with-pipeline-rasterization-line-state-create-info-ext
	   #:with-pipeline-multisample-state-create-info
	   #:with-graphics-pipeline-create-info
	   #:with-pipeline-depth-stencil-state-create-info
	   #:with-descriptor-set-layouts
	   #:with-pipeline-layout-create-info
	   #:with-pipeline-dynamic-state-create-info
	   #:with-dynamic-states
	   #:with-pipeline-color-blend-state-create-info
	   #:api-version
	   #:with-viewport
	   #:with-scissor

	   #:clampf

	   #:shift-key-down?
	   #:ctrl-key-down?
	   #:meta-key-down?
	   #:super-key-down?
	   #:mouse-down?
	   #:current-cursor-pos
	   #:previous-cursor-pos
	   #:mouse-delta))
