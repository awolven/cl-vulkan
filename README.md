# cl-vulkan
Vulkan bindings for Common Lisp.

cl-vulkan currently supports SBCL and Clozure Common Lisp on Microsoft Windows, Linux and MacOS.

cl-vulkan currently supports Vulkan 1.0 and 1.2, including compute pipelines.  Vulkan 1.1 and 1.3 are coming soon.  It is known to work on Nvidia, AMD, and Intel GPUs both discrete and integrated varieties.  It has also been tested with Swiftshader Vulkan emulator.  cl-vulkan requires the MoltenVK client driver on MacOS.  cl-vulkan is intended only to be bindings plus Common Lisp convenience layer for using Vulkan.  It has a rudimentary device memory pool but one is not required to use that.  The demo has been removed, and as a user of cl-vulkan it is expected that you know something about Vulkan and about Common Lisp and therefore will know how to employ the bindings.  Refer to the Khronos Vulkan documentation or a Vulkan tutorial for usage.

cl-vulkan relies on CLUI to provide OS Windowing support.  If you have a need to run compute pipelines entirely headless, please contact the author of cl-vulkan to enable support for that.  cl-vulkan has been run with GLFW3 and would only require minor changes to make it use GLFW/GLUT/SDL or equivalent if those are your requirements.

If you are looking for a cross-platform Common Lisp library which can readily draw graphics on the screen out of the box, take a look at KRMA instead: https://github.com/awolven/krma.git.

You must edit the file ifc/load-foreign-libs.lisp to use the Vulkan SDK.  
