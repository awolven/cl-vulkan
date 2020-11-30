# cl-vulkan
Vulkan bindings for Common Lisp with simple demo.

cl-vulkan is the new name for the vulkan-only portion of VkTk.  It extensibly wraps some of the most important Vulkan objects to run a sample application.  It includes glfw3 bindings.  It was developed and tested with SBCL.  The vulkan loader and libglfw3 are required to be present.  On Windows, cl-vulkan has been tested with an Nvidia hardware.  On macos, cl-vulkan has been tested with MoltenVK (included with the Vulkan SDK).  On Linux, a virtual machine was used, so cl-vulkan has been tested with Swiftshader software Vulkan emulation.  This cl-vulkan supersedes 3b's cl-vulkan, but uses a modified part of the bindings generator from that project.

To run cl-vulkan, clone the repository.

In SBCL:

`(push "~/cl-vulkan/" asdf:*central-registry*)`

`(asdf:oos 'asdf:load-op :cl-vulkan-demo)`

`(cl-vulkan-demo::run)`

If this does not work check to make sure load-foreign-libs.lisp has the right settings.

Enjoy!
