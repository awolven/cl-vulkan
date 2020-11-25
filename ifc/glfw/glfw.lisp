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

(cl:in-package :%glfw)

(cl:defconstant GLFW_VERSION_MAJOR 3)

(cl:defconstant GLFW_VERSION_MINOR 2)

(cl:defconstant GLFW_VERSION_REVISION 1)

(cl:defconstant GLFW_TRUE 1)

(cl:defconstant GLFW_FALSE 0)

(cl:defconstant GLFW_RELEASE 0)

(cl:defconstant GLFW_PRESS 1)

(cl:defconstant GLFW_REPEAT 2)

(cl:defconstant GLFW_KEY_UNKNOWN -1)

(cl:defconstant GLFW_KEY_SPACE 32)

(cl:defconstant GLFW_KEY_APOSTROPHE 39)

(cl:defconstant GLFW_KEY_COMMA 44)

(cl:defconstant GLFW_KEY_MINUS 45)

(cl:defconstant GLFW_KEY_PERIOD 46)

(cl:defconstant GLFW_KEY_SLASH 47)

(cl:defconstant GLFW_KEY_0 48)

(cl:defconstant GLFW_KEY_1 49)

(cl:defconstant GLFW_KEY_2 50)

(cl:defconstant GLFW_KEY_3 51)

(cl:defconstant GLFW_KEY_4 52)

(cl:defconstant GLFW_KEY_5 53)

(cl:defconstant GLFW_KEY_6 54)

(cl:defconstant GLFW_KEY_7 55)

(cl:defconstant GLFW_KEY_8 56)

(cl:defconstant GLFW_KEY_9 57)

(cl:defconstant GLFW_KEY_SEMICOLON 59)

(cl:defconstant GLFW_KEY_EQUAL 61)

(cl:defconstant GLFW_KEY_A 65)

(cl:defconstant GLFW_KEY_B 66)

(cl:defconstant GLFW_KEY_C 67)

(cl:defconstant GLFW_KEY_D 68)

(cl:defconstant GLFW_KEY_E 69)

(cl:defconstant GLFW_KEY_F 70)

(cl:defconstant GLFW_KEY_G 71)

(cl:defconstant GLFW_KEY_H 72)

(cl:defconstant GLFW_KEY_I 73)

(cl:defconstant GLFW_KEY_J 74)

(cl:defconstant GLFW_KEY_K 75)

(cl:defconstant GLFW_KEY_L 76)

(cl:defconstant GLFW_KEY_M 77)

(cl:defconstant GLFW_KEY_N 78)

(cl:defconstant GLFW_KEY_O 79)

(cl:defconstant GLFW_KEY_P 80)

(cl:defconstant GLFW_KEY_Q 81)

(cl:defconstant GLFW_KEY_R 82)

(cl:defconstant GLFW_KEY_S 83)

(cl:defconstant GLFW_KEY_T 84)

(cl:defconstant GLFW_KEY_U 85)

(cl:defconstant GLFW_KEY_V 86)

(cl:defconstant GLFW_KEY_W 87)

(cl:defconstant GLFW_KEY_X 88)

(cl:defconstant GLFW_KEY_Y 89)

(cl:defconstant GLFW_KEY_Z 90)

(cl:defconstant GLFW_KEY_LEFT_BRACKET 91)

(cl:defconstant GLFW_KEY_BACKSLASH 92)

(cl:defconstant GLFW_KEY_RIGHT_BRACKET 93)

(cl:defconstant GLFW_KEY_GRAVE_ACCENT 96)

(cl:defconstant GLFW_KEY_WORLD_1 161)

(cl:defconstant GLFW_KEY_WORLD_2 162)

(cl:defconstant GLFW_KEY_ESCAPE 256)

(cl:defconstant GLFW_KEY_ENTER 257)

(cl:defconstant GLFW_KEY_TAB 258)

(cl:defconstant GLFW_KEY_BACKSPACE 259)

(cl:defconstant GLFW_KEY_INSERT 260)

(cl:defconstant GLFW_KEY_DELETE 261)

(cl:defconstant GLFW_KEY_RIGHT 262)

(cl:defconstant GLFW_KEY_LEFT 263)

(cl:defconstant GLFW_KEY_DOWN 264)

(cl:defconstant GLFW_KEY_UP 265)

(cl:defconstant GLFW_KEY_PAGE_UP 266)

(cl:defconstant GLFW_KEY_PAGE_DOWN 267)

(cl:defconstant GLFW_KEY_HOME 268)

(cl:defconstant GLFW_KEY_END 269)

(cl:defconstant GLFW_KEY_CAPS_LOCK 280)

(cl:defconstant GLFW_KEY_SCROLL_LOCK 281)

(cl:defconstant GLFW_KEY_NUM_LOCK 282)

(cl:defconstant GLFW_KEY_PRINT_SCREEN 283)

(cl:defconstant GLFW_KEY_PAUSE 284)

(cl:defconstant GLFW_KEY_F1 290)

(cl:defconstant GLFW_KEY_F2 291)

(cl:defconstant GLFW_KEY_F3 292)

(cl:defconstant GLFW_KEY_F4 293)

(cl:defconstant GLFW_KEY_F5 294)

(cl:defconstant GLFW_KEY_F6 295)

(cl:defconstant GLFW_KEY_F7 296)

(cl:defconstant GLFW_KEY_F8 297)

(cl:defconstant GLFW_KEY_F9 298)

(cl:defconstant GLFW_KEY_F10 299)

(cl:defconstant GLFW_KEY_F11 300)

(cl:defconstant GLFW_KEY_F12 301)

(cl:defconstant GLFW_KEY_F13 302)

(cl:defconstant GLFW_KEY_F14 303)

(cl:defconstant GLFW_KEY_F15 304)

(cl:defconstant GLFW_KEY_F16 305)

(cl:defconstant GLFW_KEY_F17 306)

(cl:defconstant GLFW_KEY_F18 307)

(cl:defconstant GLFW_KEY_F19 308)

(cl:defconstant GLFW_KEY_F20 309)

(cl:defconstant GLFW_KEY_F21 310)

(cl:defconstant GLFW_KEY_F22 311)

(cl:defconstant GLFW_KEY_F23 312)

(cl:defconstant GLFW_KEY_F24 313)

(cl:defconstant GLFW_KEY_F25 314)

(cl:defconstant GLFW_KEY_KP_0 320)

(cl:defconstant GLFW_KEY_KP_1 321)

(cl:defconstant GLFW_KEY_KP_2 322)

(cl:defconstant GLFW_KEY_KP_3 323)

(cl:defconstant GLFW_KEY_KP_4 324)

(cl:defconstant GLFW_KEY_KP_5 325)

(cl:defconstant GLFW_KEY_KP_6 326)

(cl:defconstant GLFW_KEY_KP_7 327)

(cl:defconstant GLFW_KEY_KP_8 328)

(cl:defconstant GLFW_KEY_KP_9 329)

(cl:defconstant GLFW_KEY_KP_DECIMAL 330)

(cl:defconstant GLFW_KEY_KP_DIVIDE 331)

(cl:defconstant GLFW_KEY_KP_MULTIPLY 332)

(cl:defconstant GLFW_KEY_KP_SUBTRACT 333)

(cl:defconstant GLFW_KEY_KP_ADD 334)

(cl:defconstant GLFW_KEY_KP_ENTER 335)

(cl:defconstant GLFW_KEY_KP_EQUAL 336)

(cl:defconstant GLFW_KEY_LEFT_SHIFT 340)

(cl:defconstant GLFW_KEY_LEFT_CONTROL 341)

(cl:defconstant GLFW_KEY_LEFT_ALT 342)

(cl:defconstant GLFW_KEY_LEFT_SUPER 343)

(cl:defconstant GLFW_KEY_RIGHT_SHIFT 344)

(cl:defconstant GLFW_KEY_RIGHT_CONTROL 345)

(cl:defconstant GLFW_KEY_RIGHT_ALT 346)

(cl:defconstant GLFW_KEY_RIGHT_SUPER 347)

(cl:defconstant GLFW_KEY_MENU 348)

(cl:defconstant GLFW_KEY_LAST 348)

(cl:defconstant GLFW_MOD_SHIFT #x0001)

(cl:defconstant GLFW_MOD_CONTROL #x0002)

(cl:defconstant GLFW_MOD_ALT #x0004)

(cl:defconstant GLFW_MOD_SUPER #x0008)

(cl:defconstant GLFW_MOUSE_BUTTON_1 0)

(cl:defconstant GLFW_MOUSE_BUTTON_2 1)

(cl:defconstant GLFW_MOUSE_BUTTON_3 2)

(cl:defconstant GLFW_MOUSE_BUTTON_4 3)

(cl:defconstant GLFW_MOUSE_BUTTON_5 4)

(cl:defconstant GLFW_MOUSE_BUTTON_6 5)

(cl:defconstant GLFW_MOUSE_BUTTON_7 6)

(cl:defconstant GLFW_MOUSE_BUTTON_8 7)

(cl:defconstant GLFW_MOUSE_BUTTON_LAST 7)

(cl:defconstant GLFW_MOUSE_BUTTON_LEFT 0)

(cl:defconstant GLFW_MOUSE_BUTTON_RIGHT 1)

(cl:defconstant GLFW_MOUSE_BUTTON_MIDDLE 2)

(cl:defconstant GLFW_JOYSTICK_1 0)

(cl:defconstant GLFW_JOYSTICK_2 1)

(cl:defconstant GLFW_JOYSTICK_3 2)

(cl:defconstant GLFW_JOYSTICK_4 3)

(cl:defconstant GLFW_JOYSTICK_5 4)

(cl:defconstant GLFW_JOYSTICK_6 5)

(cl:defconstant GLFW_JOYSTICK_7 6)

(cl:defconstant GLFW_JOYSTICK_8 7)

(cl:defconstant GLFW_JOYSTICK_9 8)

(cl:defconstant GLFW_JOYSTICK_10 9)

(cl:defconstant GLFW_JOYSTICK_11 10)

(cl:defconstant GLFW_JOYSTICK_12 11)

(cl:defconstant GLFW_JOYSTICK_13 12)

(cl:defconstant GLFW_JOYSTICK_14 13)

(cl:defconstant GLFW_JOYSTICK_15 14)

(cl:defconstant GLFW_JOYSTICK_16 15)

(cl:defconstant GLFW_JOYSTICK_LAST 15)

(cl:defconstant GLFW_NOT_INITIALIZED #x00010001)

(cl:defconstant GLFW_NO_CURRENT_CONTEXT #x00010002)

(cl:defconstant GLFW_INVALID_ENUM #x00010003)

(cl:defconstant GLFW_INVALID_VALUE #x00010004)

(cl:defconstant GLFW_OUT_OF_MEMORY #x00010005)

(cl:defconstant GLFW_API_UNAVAILABLE #x00010006)

(cl:defconstant GLFW_VERSION_UNAVAILABLE #x00010007)

(cl:defconstant GLFW_PLATFORM_ERROR #x00010008)

(cl:defconstant GLFW_FORMAT_UNAVAILABLE #x00010009)

(cl:defconstant GLFW_NO_WINDOW_CONTEXT #x0001000A)

(cl:defconstant GLFW_FOCUSED #x00020001)

(cl:defconstant GLFW_ICONIFIED #x00020002)

(cl:defconstant GLFW_RESIZABLE #x00020003)

(cl:defconstant GLFW_VISIBLE #x00020004)

(cl:defconstant GLFW_DECORATED #x00020005)

(cl:defconstant GLFW_AUTO_ICONIFY #x00020006)

(cl:defconstant GLFW_FLOATING #x00020007)

(cl:defconstant GLFW_MAXIMIZED #x00020008)

(cl:defconstant GLFW_FOCUS_ON_SHOW #x0002000C)

(cl:defconstant GLFW_RED_BITS #x00021001)

(cl:defconstant GLFW_GREEN_BITS #x00021002)

(cl:defconstant GLFW_BLUE_BITS #x00021003)

(cl:defconstant GLFW_ALPHA_BITS #x00021004)

(cl:defconstant GLFW_DEPTH_BITS #x00021005)

(cl:defconstant GLFW_STENCIL_BITS #x00021006)

(cl:defconstant GLFW_ACCUM_RED_BITS #x00021007)

(cl:defconstant GLFW_ACCUM_GREEN_BITS #x00021008)

(cl:defconstant GLFW_ACCUM_BLUE_BITS #x00021009)

(cl:defconstant GLFW_ACCUM_ALPHA_BITS #x0002100A)

(cl:defconstant GLFW_AUX_BUFFERS #x0002100B)

(cl:defconstant GLFW_STEREO #x0002100C)

(cl:defconstant GLFW_SAMPLES #x0002100D)

(cl:defconstant GLFW_SRGB_CAPABLE #x0002100E)

(cl:defconstant GLFW_REFRESH_RATE #x0002100F)

(cl:defconstant GLFW_DOUBLEBUFFER #x00021010)

(cl:defconstant GLFW_CLIENT_API #x00022001)

(cl:defconstant GLFW_CONTEXT_VERSION_MAJOR #x00022002)

(cl:defconstant GLFW_CONTEXT_VERSION_MINOR #x00022003)

(cl:defconstant GLFW_CONTEXT_REVISION #x00022004)

(cl:defconstant GLFW_CONTEXT_ROBUSTNESS #x00022005)

(cl:defconstant GLFW_OPENGL_FORWARD_COMPAT #x00022006)

(cl:defconstant GLFW_OPENGL_DEBUG_CONTEXT #x00022007)

(cl:defconstant GLFW_OPENGL_PROFILE #x00022008)

(cl:defconstant GLFW_CONTEXT_RELEASE_BEHAVIOR #x00022009)

(cl:defconstant GLFW_CONTEXT_NO_ERROR #x0002200A)

(cl:defconstant GLFW_CONTEXT_CREATION_API #x0002200B)

(cl:defconstant GLFW_NO_API 0)

(cl:defconstant GLFW_OPENGL_API #x00030001)

(cl:defconstant GLFW_OPENGL_ES_API #x00030002)

(cl:defconstant GLFW_NO_ROBUSTNESS 0)

(cl:defconstant GLFW_NO_RESET_NOTIFICATION #x00031001)

(cl:defconstant GLFW_LOSE_CONTEXT_ON_RESET #x00031002)

(cl:defconstant GLFW_OPENGL_ANY_PROFILE 0)

(cl:defconstant GLFW_OPENGL_CORE_PROFILE #x00032001)

(cl:defconstant GLFW_OPENGL_COMPAT_PROFILE #x00032002)

(cl:defconstant GLFW_CURSOR #x00033001)

(cl:defconstant GLFW_STICKY_KEYS #x00033002)

(cl:defconstant GLFW_STICKY_MOUSE_BUTTONS #x00033003)

(cl:defconstant GLFW_CURSOR_NORMAL #x00034001)

(cl:defconstant GLFW_CURSOR_HIDDEN #x00034002)

(cl:defconstant GLFW_CURSOR_DISABLED #x00034003)

(cl:defconstant GLFW_ANY_RELEASE_BEHAVIOR 0)

(cl:defconstant GLFW_RELEASE_BEHAVIOR_FLUSH #x00035001)

(cl:defconstant GLFW_RELEASE_BEHAVIOR_NONE #x00035002)

(cl:defconstant GLFW_NATIVE_CONTEXT_API #x00036001)

(cl:defconstant GLFW_EGL_CONTEXT_API #x00036002)

(cl:defconstant GLFW_ARROW_CURSOR #x00036001)

(cl:defconstant GLFW_IBEAM_CURSOR #x00036002)

(cl:defconstant GLFW_CROSSHAIR_CURSOR #x00036003)

(cl:defconstant GLFW_HAND_CURSOR #x00036004)

(cl:defconstant GLFW_HRESIZE_CURSOR #x00036005)

(cl:defconstant GLFW_VRESIZE_CURSOR #x00036006)

(cl:defconstant GLFW_CONNECTED #x00040001)

(cl:defconstant GLFW_DISCONNECTED #x00040002)

(cl:defconstant GLFW_DONT_CARE -1)

(cffi:defcstruct GLFWvidmode
	(width :int)
	(height :int)
	(redBits :int)
	(greenBits :int)
	(blueBits :int)
	(refreshRate :int))

(cffi:defcstruct GLFWgammaramp
	(red :pointer)
	(green :pointer)
	(blue :pointer)
	(size :unsigned-int))

(cffi:defcstruct GLFWimage
	(width :int)
	(height :int)
	(pixels :pointer))

(cffi:defcstruct GLFWmonitor)

(cffi:defcfun ("glfwInit" glfwInit) :int)

(cffi:defcfun ("glfwTerminate" glfwTerminate) :void)

(cffi:defcfun ("glfwGetVersion" glfwGetVersion) :void
  (major :pointer)
  (minor :pointer)
  (rev :pointer))

(cffi:defcfun ("glfwGetVersionString" glfwGetVersionString) :string)

(cffi:defcfun ("glfwSetErrorCallback" glfwSetErrorCallback) :pointer
  (cbfun :pointer))

(cffi:defcfun ("glfwGetMonitors" glfwGetMonitors) :pointer
  (count :pointer))

(cffi:defcfun ("glfwGetPrimaryMonitor" glfwGetPrimaryMonitor) :pointer)

(cffi:defcfun ("glfwGetMonitorPos" glfwGetMonitorPos) :void
  (monitor :pointer)
  (xpos :pointer)
  (ypos :pointer))

(cffi:defcfun ("glfwGetMonitorPhysicalSize" glfwGetMonitorPhysicalSize) :void
  (monitor :pointer)
  (widthMM :pointer)
  (heightMM :pointer))

(cffi:defcfun ("glfwGetMonitorName" glfwGetMonitorName) :string
  (monitor :pointer))

(cffi:defcfun ("glfwSetMonitorCallback" glfwSetMonitorCallback) :pointer
  (cbfun :pointer))

(cffi:defcfun ("glfwGetVideoModes" glfwGetVideoModes) :pointer
  (monitor :pointer)
  (count :pointer))

(cffi:defcfun ("glfwGetVideoMode" glfwGetVideoMode) :pointer
  (monitor :pointer))

(cffi:defcfun ("glfwSetGamma" glfwSetGamma) :void
  (monitor :pointer)
  (gamma :float))

(cffi:defcfun ("glfwGetGammaRamp" glfwGetGammaRamp) :pointer
  (monitor :pointer))

(cffi:defcfun ("glfwSetGammaRamp" glfwSetGammaRamp) :void
  (monitor :pointer)
  (ramp :pointer))

(cffi:defcfun ("glfwDefaultWindowHints" glfwDefaultWindowHints) :void)

(cffi:defcfun ("glfwWindowHint" glfwWindowHint) :void
  (hint :int)
  (value :int))

(cffi:defcfun ("glfwCreateWindow" glfwCreateWindow) :pointer
  (width :int)
  (height :int)
  (title :string)
  (monitor :pointer)
  (share :pointer))

(cffi:defcfun ("glfwDestroyWindow" glfwDestroyWindow) :void
  (window :pointer))

(cffi:defcfun ("glfwWindowShouldClose" glfwWindowShouldClose) :int
  (window :pointer))

(cffi:defcfun ("glfwSetWindowShouldClose" glfwSetWindowShouldClose) :void
  (window :pointer)
  (value :int))

(cffi:defcfun ("glfwSetWindowTitle" glfwSetWindowTitle) :void
  (window :pointer)
  (title :string))

(cffi:defcfun ("glfwSetWindowIcon" glfwSetWindowIcon) :void
  (window :pointer)
  (count :int)
  (images :pointer))

(cffi:defcfun ("glfwGetWindowPos" glfwGetWindowPos) :void
  (window :pointer)
  (xpos :pointer)
  (ypos :pointer))

(cffi:defcfun ("glfwSetWindowPos" glfwSetWindowPos) :void
  (window :pointer)
  (xpos :int)
  (ypos :int))

(cffi:defcfun ("glfwGetWindowSize" glfwGetWindowSize) :void
  (window :pointer)
  (width :pointer)
  (height :pointer))

(cffi:defcfun ("glfwSetWindowSizeLimits" glfwSetWindowSizeLimits) :void
  (window :pointer)
  (minwidth :int)
  (minheight :int)
  (maxwidth :int)
  (maxheight :int))

(cffi:defcfun ("glfwSetWindowAspectRatio" glfwSetWindowAspectRatio) :void
  (window :pointer)
  (numer :int)
  (denom :int))

(cffi:defcfun ("glfwSetWindowSize" glfwSetWindowSize) :void
  (window :pointer)
  (width :int)
  (height :int))

(cffi:defcfun ("glfwGetFramebufferSize" glfwGetFramebufferSize) :void
  (window :pointer)
  (width :pointer)
  (height :pointer))

(cffi:defcfun ("glfwGetWindowFrameSize" glfwGetWindowFrameSize) :void
  (window :pointer)
  (left :pointer)
  (top :pointer)
  (right :pointer)
  (bottom :pointer))

(cffi:defcfun ("glfwIconifyWindow" glfwIconifyWindow) :void
  (window :pointer))

(cffi:defcfun ("glfwRestoreWindow" glfwRestoreWindow) :void
  (window :pointer))

(cffi:defcfun ("glfwMaximizeWindow" glfwMaximizeWindow) :void
  (window :pointer))

(cffi:defcfun ("glfwShowWindow" glfwShowWindow) :void
  (window :pointer))

(cffi:defcfun ("glfwHideWindow" glfwHideWindow) :void
  (window :pointer))

(cffi:defcfun ("glfwFocusWindow" glfwFocusWindow) :void
  (window :pointer))

(cffi:defcfun ("glfwGetWindowMonitor" glfwGetWindowMonitor) :pointer
  (window :pointer))

(cffi:defcfun ("glfwSetWindowMonitor" glfwSetWindowMonitor) :void
  (window :pointer)
  (monitor :pointer)
  (xpos :int)
  (ypos :int)
  (width :int)
  (height :int)
  (refreshRate :int))

(cffi:defcfun ("glfwGetWindowAttrib" glfwGetWindowAttrib) :int
  (window :pointer)
  (attrib :int))

(cffi:defcfun ("glfwSetWindowUserPointer" glfwSetWindowUserPointer) :void
  (window :pointer)
  (pointer :pointer))

(cffi:defcfun ("glfwGetWindowUserPointer" glfwGetWindowUserPointer) :pointer
  (window :pointer))

(cffi:defcfun ("glfwSetWindowPosCallback" glfwSetWindowPosCallback) :pointer
  (window :pointer)
  (cbfun :pointer))

(cffi:defcfun ("glfwSetWindowSizeCallback" glfwSetWindowSizeCallback) :pointer
  (window :pointer)
  (cbfun :pointer))

(cffi:defcfun ("glfwSetWindowCloseCallback" glfwSetWindowCloseCallback) :pointer
  (window :pointer)
  (cbfun :pointer))

(cffi:defcfun ("glfwSetWindowRefreshCallback" glfwSetWindowRefreshCallback) :pointer
  (window :pointer)
  (cbfun :pointer))

(cffi:defcfun ("glfwSetWindowFocusCallback" glfwSetWindowFocusCallback) :pointer
  (window :pointer)
  (cbfun :pointer))

(cffi:defcfun ("glfwSetWindowIconifyCallback" glfwSetWindowIconifyCallback) :pointer
  (window :pointer)
  (cbfun :pointer))

(cffi:defcfun ("glfwSetFramebufferSizeCallback" glfwSetFramebufferSizeCallback) :pointer
  (window :pointer)
  (cbfun :pointer))

(cffi:defcfun ("glfwPollEvents" glfwPollEvents) :void)

(cffi:defcfun ("glfwWaitEvents" glfwWaitEvents) :void)

(cffi:defcfun ("glfwWaitEventsTimeout" glfwWaitEventsTimeout) :void
  (timeout :double))

(cffi:defcfun ("glfwPostEmptyEvent" glfwPostEmptyEvent) :void)

(cffi:defcfun ("glfwGetInputMode" glfwGetInputMode) :int
  (window :pointer)
  (mode :int))

(cffi:defcfun ("glfwSetInputMode" glfwSetInputMode) :void
  (window :pointer)
  (mode :int)
  (value :int))

(cffi:defcfun ("glfwGetKeyName" glfwGetKeyName) :string
  (key :int)
  (scancode :int))

(cffi:defcfun ("glfwGetKey" glfwGetKey) :int
  (window :pointer)
  (key :int))

(cffi:defcfun ("glfwGetMouseButton" glfwGetMouseButton) :int
  (window :pointer)
  (button :int))

(cffi:defcfun ("glfwGetCursorPos" glfwGetCursorPos) :void
  (window :pointer)
  (xpos :pointer)
  (ypos :pointer))

(cffi:defcfun ("glfwSetCursorPos" glfwSetCursorPos) :void
  (window :pointer)
  (xpos :double)
  (ypos :double))

(cffi:defcfun ("glfwCreateCursor" glfwCreateCursor) :pointer
  (image :pointer)
  (xhot :int)
  (yhot :int))

(cffi:defcfun ("glfwCreateStandardCursor" glfwCreateStandardCursor) :pointer
  (shape :int))

(cffi:defcfun ("glfwDestroyCursor" glfwDestroyCursor) :void
  (cursor :pointer))

(cffi:defcfun ("glfwSetCursor" glfwSetCursor) :void
  (window :pointer)
  (cursor :pointer))

(cffi:defcfun ("glfwSetKeyCallback" glfwSetKeyCallback) :pointer
  (window :pointer)
  (cbfun :pointer))

(cffi:defcfun ("glfwSetCharCallback" glfwSetCharCallback) :pointer
  (window :pointer)
  (cbfun :pointer))

(cffi:defcfun ("glfwSetCharModsCallback" glfwSetCharModsCallback) :pointer
  (window :pointer)
  (cbfun :pointer))

(cffi:defcfun ("glfwSetMouseButtonCallback" glfwSetMouseButtonCallback) :pointer
  (window :pointer)
  (cbfun :pointer))

(cffi:defcfun ("glfwSetCursorPosCallback" glfwSetCursorPosCallback) :pointer
  (window :pointer)
  (cbfun :pointer))

(cffi:defcfun ("glfwSetCursorEnterCallback" glfwSetCursorEnterCallback) :pointer
  (window :pointer)
  (cbfun :pointer))

(cffi:defcfun ("glfwSetScrollCallback" glfwSetScrollCallback) :pointer
  (window :pointer)
  (cbfun :pointer))

(cffi:defcfun ("glfwSetDropCallback" glfwSetDropCallback) :pointer
  (window :pointer)
  (cbfun :pointer))

(cffi:defcfun ("glfwJoystickPresent" glfwJoystickPresent) :int
  (joy :int))

(cffi:defcfun ("glfwGetJoystickAxes" glfwGetJoystickAxes) :pointer
  (joy :int)
  (count :pointer))

(cffi:defcfun ("glfwGetJoystickButtons" glfwGetJoystickButtons) :pointer
  (joy :int)
  (count :pointer))

(cffi:defcfun ("glfwGetJoystickName" glfwGetJoystickName) :string
  (joy :int))

(cffi:defcfun ("glfwSetJoystickCallback" glfwSetJoystickCallback) :pointer
  (cbfun :pointer))

(cffi:defcfun ("glfwSetClipboardString" glfwSetClipboardString) :void
  (window :pointer)
  (string :string))

(cffi:defcfun ("glfwGetClipboardString" glfwGetClipboardString) :string
  (window :pointer))

(cffi:defcfun ("glfwGetTime" glfwGetTime) :double)

(cffi:defcfun ("glfwSetTime" glfwSetTime) :void
  (time :double))

(cffi:defcfun ("glfwGetTimerValue" glfwGetTimerValue) :pointer)

(cffi:defcfun ("glfwGetTimerFrequency" glfwGetTimerFrequency) :pointer)

(cffi:defcfun ("glfwMakeContextCurrent" glfwMakeContextCurrent) :void
  (window :pointer))

(cffi:defcfun ("glfwGetCurrentContext" glfwGetCurrentContext) :pointer)

(cffi:defcfun ("glfwSwapBuffers" glfwSwapBuffers) :void
  (window :pointer))

(cffi:defcfun ("glfwSwapInterval" glfwSwapInterval) :void
  (interval :int))

(cffi:defcfun ("glfwExtensionSupported" glfwExtensionSupported) :int
  (extension :string))

(cffi:defcfun ("glfwGetProcAddress" glfwGetProcAddress) :pointer
  (procname :string))

(cffi:defcfun ("glfwVulkanSupported" glfwVulkanSupported) :int)

(cffi:defcfun ("glfwGetRequiredInstanceExtensions" glfwGetRequiredInstanceExtensions) :pointer
  (count :pointer))
