;; Copyright 2019, 2020 Andrew Kenneth Wolven <awolven@gmail.com>
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

(defsystem cl-vulkan
  :description "Bindings for using Vulkan with Common Lisp"
  :depends-on (:cffi :bordeaux-threads #+noglfw :abstract-os)
  :author "Andrew K Wolven <awolven@gmail.com>"
  :components
  ((:file "features")
   (:file "ifc/glfw/package")
   (:file "ifc/glfw/glfw")
   (:file "ifc/vulkan/package")
   (:file "ifc/vulkan/vk-types")
   (:file "ifc/vulkan/s-type-table")
   (:file "ifc/vulkan/vk-macros")
   (:file "ifc/vulkan/vk-funcs")
   (:file "src/package")
   (:file "src/utilities")
   (:file "src/macros")
   (:file "src/support")
   (:file "src/helpers")
   (:file "src/classes")
   (:file "src/allocation-callbacks")
   (:file "src/debug-report")
   (:file "src/pipeline-cache")
   (:file "src/vulkan-instance")
   (:file "src/physical-device")
   (:file "src/queue-family")
   (:file "src/memory-type")
   (:file "src/memory-heap")
   (:file "src/logical-device")
   (:file "src/fence")
   (:file "src/swapchain")
   (:file "src/memory-pool")
   (:file "src/vulkan-application")
   (:file "src/window")
   (:file "src/surface-format")
   (:file "src/surface-capabilities")
   (:file "src/present-modes")
   (:file "src/surface")
   (:file "src/queue")
   (:file "src/images")
   (:file "src/image-views")
   (:file "src/render-pass")
   (:file "src/descriptor-set-layout")
   (:file "src/pipeline-layout")
   (:file "src/shader-module")
   (:file "src/graphics-pipeline")
   (:file "src/compute-pipeline")
   (:file "src/command-pool")
   (:file "src/framebuffer")
   (:file "src/buffers")
   (:file "src/descriptor-pool")
   (:file "src/descriptor-sets")
   (:file "src/command-buffers")
   (:file "src/sampler")
   (:file "src/spirv")
   (:file "ifc/load-foreign-libs")))
  
