(in-package :cl-vulkan-demo)

(defconstant +max-frames+ 4)

(defconstant +buffer-alignment+ 256)

(defconstant +fnv-float-big+ #.(* 3 4096))
(defconstant +fnv-float-huge+ #.(* 24 4096))
(defconstant +fnv-float-ginormous+ #.(* 65536 8))

(defconstant +fnv-ushort-big+ 4096)
(defconstant +fnv-ushort-huge+ 32768)
(defconstant +fnv-ushort-ginormous+ 65536)

(defparameter +black+ (make-array 4 :element-type 'single-float :initial-contents (list 0.0f0 0.0f0 0.0f0 1.0f0)))
(defparameter +white+ (make-array 4 :element-type 'single-float :initial-contents (list 1.0f0 1.0f0 1.0f0 1.0f0)))
(defparameter +grey+ (make-array 4 :element-type 'single-float :initial-contents (list 0.25f0 0.25f0 0.25f0 1.0f0)))
(defparameter +red+ (make-array 4 :element-type 'single-float :initial-contents (list 1.0f0 0.0f0 0.0f0 1.0f0)))

;; the matrix below is used to emulate OpenGL for the 3d-matrices library.
(defparameter +clip-matrix+
  (mat 1 0 0 0
       0 -1 0 0
       0 0 1/2 0
       0 0 1/2 1))

(defcstruct mat4
  (x0 :float)
  (y0 :float)
  (z0 :float)
  (w0 :float)
  (x1 :float)
  (y1 :float)
  (z1 :float)
  (w1 :float)
  (x2 :float)
  (y2 :float)
  (z2 :float)
  (w2 :float)
  (x3 :float)
  (y3 :float)
  (z3 :float)
  (w3 :float))

(defcstruct 3DMatrices
  (model (:struct mat4))
  (view (:struct mat4))
  (proj (:struct mat4))
  (clip (:struct mat4)))

(defcstruct simple-3d-vertex
  (x :float)
  (y :float)
  (z :float))

(defcstruct 3DColorVertex
  (position (:struct simple-3d-vertex))
  (color :float :count 4))

(defclass foreign-adjustable-numeric-vector ()
  ((array-pointer :accessor array-pointer :initarg :array-pointer)
   (fill-pointer :accessor array-fill-pointer :initform 0)
   (allocated-size :accessor array-allocated-size :initarg :allocated-size)))

(defun fnv-push-extend (number numeric-vector)
  (let ((foreign-type (etypecase number
			((integer 0 65535) :unsigned-short)
			;;((integer 65536) :int)  ;; this won't fill int arrays correctly for indices < 65536
			(single-float :float)
			(double-float :double))))
  (with-slots (allocated-size fill-pointer) numeric-vector
    (if (< fill-pointer allocated-size)
	(progn (setf (mem-aref (array-pointer numeric-vector) foreign-type fill-pointer) number)
	       (incf fill-pointer)
	       (values))
	(let* ((new-size (* 2 allocated-size))
	       (new-array (foreign-alloc foreign-type :count new-size))
	       (old-array (array-pointer numeric-vector)))
	  (vk::memcpy new-array old-array (* fill-pointer (foreign-type-size foreign-type)))
	  (setf (array-pointer numeric-vector) new-array)
	  (setf allocated-size new-size)
	  (foreign-free old-array)
	  (setf (mem-aref new-array foreign-type fill-pointer) number)
	  (incf fill-pointer)
	  (values))))))

(defmethod destroy-fnv (fnv)
  (foreign-free (array-pointer fnv)))

(defmethod initialize-instance :after ((instance foreign-adjustable-numeric-vector) &rest initargs
				       &key initial-size element-type)
  (declare (ignore initargs))
  (with-slots (array-pointer allocated-size) instance
    (setf array-pointer (foreign-alloc element-type :count initial-size)
	  allocated-size initial-size)

    (values)))

(defclass vertex-array-mixin () ())

(defclass vertex-array (vertex-array-mixin foreign-adjustable-numeric-vector)
  ())

(defmethod initialize-instance :around ((instance vertex-array) &rest initargs
					&key (initial-size +fnv-float-big+)
					  (element-type :float))
  (declare (ignore initargs))
  (assert (eq :float element-type))
  (call-next-method instance :initial-size initial-size :element-type element-type))

(defclass index-array-mixin () ())

(defclass index-array-ushort (index-array-mixin foreign-adjustable-numeric-vector)
  ())

(defmethod initialize-instance :around ((instance index-array-ushort) &rest initargs
					&key (initial-size +fnv-ushort-big+)
					  (element-type :unsigned-short))
  (declare (ignore initargs))
  (assert (eq :unsigned-short element-type))
  (call-next-method instance :initial-size initial-size :element-type element-type))

(defclass draw-list ()
  ((vertex-array :accessor draw-list-vertex-array)
   (renderer :initarg :renderer :reader draw-list-renderer)))

(defclass point-draw-list (draw-list) ())

(defclass indexed-draw-list (draw-list)
  ((index-array :accessor draw-list-index-array)
   (commands :accessor draw-commands :initform (make-array 100 :adjustable t :fill-pointer 0))))


(defmethod initialize-instance :after ((instance draw-list) &rest initargs
				       &key (initial-size +fnv-float-big+))
  (declare (ignore initargs))
  (setf (draw-list-vertex-array instance)
	(make-instance 'vertex-array :initial-size initial-size))
  (values))

(defmethod initialize-instance :after ((instance indexed-draw-list) &rest initargs
				       &key (initial-size +fnv-ushort-big+))
  (declare (ignore initargs))
  (setf (draw-list-index-array instance)
	(make-instance 'index-array-ushort :initial-size initial-size))
  (values))

(defclass renderer-mixin ()
  ((application :reader application :initarg :app)
   (name :initarg :name :reader renderer-name)
   (descriptor-set-layout :accessor descriptor-set-layout)
   (descriptor-set :accessor descriptor-set)
   (pipeline-layout :accessor pipeline-layout)
   (pipeline :accessor pipeline)
   (uniform-buffer :accessor uniform-buffer)
   (uniform-buffer-stage :accessor uniform-buffer-stage)
   (draw-list :accessor renderer-draw-list)
   (frame-resources :initform (make-array +max-frames+ :initial-element nil))
   (invalidated? :accessor invalidated? :initform t)))

(defmethod destroy-renderer ((renderer renderer-mixin))

  (with-slots (frame-resources) renderer
    (loop for frame-resource across frame-resources
       when frame-resource
       do
	 (destroy-buffer (vertex-buffer frame-resource))
	 (destroy-buffer (index-buffer frame-resource))))

  (destroy-buffer (uniform-buffer renderer))
  (foreign-free (uniform-buffer-stage renderer))

  (destroy-fnv (draw-list-vertex-array (renderer-draw-list renderer)))

  (destroy-descriptor-set-layout (descriptor-set-layout renderer))
  (vkDestroyDescriptorPool (h (default-logical-device renderer)) (h (descriptor-pool renderer)) +nullptr+)

  (destroy-pipeline (pipeline renderer))
  (destroy-pipeline-layout (pipeline-layout renderer))
  (values))

(defmethod main-window ((renderer renderer-mixin))
  (with-slots (application) renderer
    (main-window application)))

(defmethod allocator ((renderer renderer-mixin))
  (with-slots (application) renderer
    (allocator application)))

(defmethod default-logical-device ((renderer renderer-mixin))
  (with-slots (application) renderer
    (default-logical-device application)))

(defmethod pipeline-cache ((renderer renderer-mixin))
  (with-slots (application) renderer
    (pipeline-cache application)))

(defmethod descriptor-pool ((renderer renderer-mixin))
  (with-slots (application) renderer
    (default-descriptor-pool application)))

(defmethod render-pass ((renderer renderer-mixin))
  (render-pass (swapchain (main-window renderer))))

(defun create-standard-renderer-device-objects (renderer
						&key
						  (render-pass (render-pass renderer))
						  (bindings (make-descriptor-set-layout-bindings renderer))
						  (push-constant-ranges (make-push-constant-ranges renderer))
						  (line-width (renderer-line-width renderer))
						  (vertex-type (pipeline-vertex-type renderer))
						  (vertex-input-attribute-descriptions
						   (make-vertex-input-attribute-descriptions renderer))
						  (topology (pipeline-topology renderer))
						  (min-sample-shading (pipeline-min-sample-shading renderer))
						  (depth-test-enable (pipeline-depth-test-enable? renderer))
						  (depth-write-enable (pipeline-depth-write-enable? renderer))
						  (depth-compare-op (pipeline-depth-compare-op renderer))
						  (logic-op (pipeline-logic-op renderer))
						  (blend-enable (pipeline-blend-enable? renderer))
						  (depth-clamp-enable (pipeline-depth-clamp-enable? renderer))
						  (src-alpha-blend-factor (pipeline-src-alpha-blend-factor renderer))
						  (stippled-line-enable nil)
						  (additional-pipeline-creation-args nil))
  (let ((device (default-logical-device renderer)))
    (let ((vtx-shader (create-shader-module-from-file device (vertex-shader-pathname renderer)))
	  (frg-shader (create-shader-module-from-file device (fragment-shader-pathname renderer))))

	(setf (descriptor-set-layout renderer) (create-descriptor-set-layout device :bindings bindings))

	(setf (pipeline-layout renderer)
	      (create-pipeline-layout device (list (descriptor-set-layout renderer))
				      :push-constant-ranges push-constant-ranges)
	      (pipeline renderer)
	      (apply #'create-graphics-pipeline device (pipeline-cache renderer) (pipeline-layout renderer)
		     render-pass 3 vtx-shader frg-shader
		     :line-width line-width
		     :vertex-type vertex-type
		     :vertex-input-attribute-descriptions vertex-input-attribute-descriptions
		     :topology topology
		     :min-sample-shading min-sample-shading
		     :depth-test-enable (if depth-test-enable (if (eq depth-test-enable VK_FALSE)
								  VK_FALSE VK_TRUE)
					    VK_FALSE)
		     :depth-write-enable (if depth-write-enable (if (eq depth-write-enable VK_FALSE)
								    VK_FALSE VK_TRUE)
					     VK_FALSE)
		     :depth-compare-op depth-compare-op
		     :logic-op logic-op
		     :blend-enable (if blend-enable (if (eq blend-enable VK_FALSE) VK_FALSE VK_TRUE) VK_FALSE)
		     :depth-clamp-enable (if depth-clamp-enable (if (eq depth-clamp-enable VK_FALSE)
								    VK_FALSE VK_TRUE)
					     VK_FALSE)
		     :src-alpha-blend-factor src-alpha-blend-factor
		     :stippled-line-enable (if stippled-line-enable
					       VK_TRUE VK_FALSE)
		     :allocator (allocator renderer)
		     additional-pipeline-creation-args))
	      

	(destroy-shader-module vtx-shader)
	(destroy-shader-module frg-shader)
	  
	(setf (uniform-buffer renderer)
	      (create-uniform-buffer device (foreign-type-size (uniform-buffer-type renderer))))

	(setf (uniform-buffer-stage renderer)
	      (foreign-alloc (uniform-buffer-type renderer)))
	  
	(setf (descriptor-set renderer)
	      (create-descriptor-set
	       device
	       (list (descriptor-set-layout renderer))
	       (descriptor-pool renderer)
	       :descriptor-buffer-info
	       (make-descriptor-buffer-info renderer))))))

(defgeneric pipeline-topology (renderer))

(defgeneric vertex-shader-pathname (renderer))

(defgeneric fragment-shader-pathname (renderer))

(defmethod renderer-line-width ((renderer renderer-mixin))
  #+(or windows linux) 2.0f0
  #+darwin 1.0f0)

(defgeneric create-device-objects (renderer))

(defmethod create-device-objects ((renderer renderer-mixin))
  (create-standard-renderer-device-objects renderer))

(defmethod initialize-instance :after ((renderer renderer-mixin) &rest initargs
								   &key)
  (declare (ignore initargs))
  (create-device-objects renderer)
  (values))

(defmethod make-descriptor-set-layout-bindings ((renderer renderer-mixin))
  (list (make-instance 'uniform-buffer-for-vertex-shader-dsl-binding)))

(defmethod make-push-constant-ranges ((renderer renderer-mixin))
  nil)

(defmethod pipeline-vertex-type ((renderer renderer-mixin))
  '(:struct 3DColorVertex))

(defun vertex-float-size (renderer)
  (/ (foreign-type-size (pipeline-vertex-type renderer))
     #.(foreign-type-size :float)))

(defmethod uniform-buffer-type ((renderer renderer-mixin))
  '(:struct 3DMatrices))

(defmethod make-vertex-input-attribute-descriptions ((renderer renderer-mixin))
  (list (make-instance 'vertex-input-attribute-description
		       :location 0
		       :format VK_FORMAT_R32G32B32_SFLOAT
		       :offset (foreign-slot-offset (pipeline-vertex-type renderer) 'position))
	(make-instance 'vertex-input-attribute-description
		       :location 1
		       :format VK_FORMAT_R32G32B32A32_SFLOAT
		       :offset (foreign-slot-offset (pipeline-vertex-type renderer) 'color))))

(defmethod pipeline-depth-test-enable? ((renderer renderer-mixin))
  t)

(defmethod pipeline-depth-write-enable? ((renderer renderer-mixin))
  t)

(defmethod pipeline-depth-compare-op ((renderer renderer-mixin))
  VK_COMPARE_OP_LESS)

(defmethod pipeline-logic-op ((renderer renderer-mixin))
  VK_LOGIC_OP_COPY)

(defmethod pipeline-blend-enable? ((renderer renderer-mixin))
  t)
  
(defmethod pipeline-depth-clamp-enable? ((renderer renderer-mixin))
  nil)

(defmethod pipeline-src-alpha-blend-factor ((renderer renderer-mixin))
  VK_BLEND_FACTOR_ONE)

(defmethod pipeline-min-sample-shading ((renderer renderer-mixin))
  1.0f0)

(defmethod make-descriptor-buffer-info ((renderer renderer-mixin))
    (list (make-instance 'descriptor-uniform-buffer-info
			 :buffer (uniform-buffer renderer)
			 :range (foreign-type-size (uniform-buffer-type renderer)))))

(defclass draw-indexed-renderer-mixin (renderer-mixin) ())

(defmethod initialize-instance :after ((renderer draw-indexed-renderer-mixin) &rest initargs
								   &key)
  (declare (ignore initargs))
  (setf (renderer-draw-list renderer) (make-instance 'indexed-draw-list :renderer renderer))
  (values))

(defmethod destroy-renderer ((renderer draw-indexed-renderer-mixin))
  (destroy-fnv (draw-list-index-array (renderer-draw-list renderer)))
  (call-next-method))

(defclass triangle-list-renderer (draw-indexed-renderer-mixin)
  ())
(defmethod pipeline-topology ((renderer triangle-list-renderer))
  VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST)

(defmethod vertex-shader-pathname ((renderer triangle-list-renderer))
  (asdf/system:system-relative-pathname :cl-vulkan-demo "shaders/vert.spv"))

(defmethod fragment-shader-pathname ((renderer triangle-list-renderer))
  (asdf/system:system-relative-pathname :cl-vulkan-demo "shaders/frag.spv"))

(defclass vertex-buffer-slot-mixin ()
  ((vertex-buffer :initform nil :accessor vertex-buffer)))

(defclass index-buffer-slot-mixin ()
  ((index-buffer :initform nil :accessor index-buffer)))

(defclass draw-only-frame-resource (vertex-buffer-slot-mixin)
  ())

(defclass draw-indexed-frame-resource (vertex-buffer-slot-mixin
				       index-buffer-slot-mixin)
  ())

(defmethod frame-resource-type ((renderer renderer-mixin))
  (load-time-value (find-class 'draw-only-frame-resource)))

(defmethod frame-resource-type ((renderer draw-indexed-renderer-mixin))
  (load-time-value (find-class 'draw-indexed-frame-resource)))

(defmethod get-frame-resource ((renderer renderer-mixin) frame-index)
  (with-slots (frame-resources) renderer
    (if (elt frame-resources frame-index)
	(elt frame-resources frame-index)
	(setf (elt frame-resources frame-index)
	      (make-instance (frame-resource-type renderer))))))

(defmethod initialize-buffers ((renderer renderer-mixin) frame-index)
  (let ((frame-resource (get-frame-resource renderer frame-index))
	(device (default-logical-device renderer)))
    (with-slots (draw-list) renderer
      (with-slots (vertex-array) draw-list
	(with-slots (array-pointer fill-pointer) vertex-array
	  (let ((size (* fill-pointer #.(foreign-type-size :float))))
	    (with-slots (vertex-buffer) frame-resource

	      (labels ((new-buffer ()
			 (let ((new-buffer-size (* (1+ (ceiling (/ (1- size) +buffer-alignment+))) +buffer-alignment+)))
			   
			   (setf vertex-buffer
				 (create-buffer-1 device new-buffer-size VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
						  :buffer-class 'vertex-buffer :allocator (allocator renderer)))
			   (new-memory)))
		       
		       (new-memory ()
			 (let ((buffer-memory
				(allocate-buffer-memory device vertex-buffer VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
							:allocator (allocator renderer))))

			   (setf (allocated-memory vertex-buffer) buffer-memory)
			 
			   (bind-buffer-memory device vertex-buffer buffer-memory)))

		       (free-buffer ()
			 (vkDestroyBuffer (h device) (h vertex-buffer) (h (allocator vertex-buffer)))

			 (when (allocated-memory vertex-buffer)
			   (free-memory)))

		       (free-memory ()
			 (vkFreeMemory (h device) (h (allocated-memory vertex-buffer)) (h (allocator (allocated-memory vertex-buffer))))))
		
		(cond ((null vertex-buffer) (new-buffer))
		      ((or (null (allocated-memory vertex-buffer))
			   (< (vk::size vertex-buffer) size))
		       (free-buffer) (new-buffer)))

		(mmap-buffer vertex-buffer (array-pointer vertex-array) size)))))))))

	      

(defmethod initialize-buffers ((renderer draw-indexed-renderer-mixin) frame-index)
  (call-next-method)
  (let ((frame-resource (get-frame-resource renderer frame-index))
	(device (default-logical-device renderer)))
    (with-slots (draw-list) renderer
      (with-slots (index-array) draw-list
	(with-slots (array-pointer fill-pointer) index-array
	  (let ((size (* fill-pointer #.(foreign-type-size :unsigned-short))))
	    (with-slots (index-buffer) frame-resource
	      
	      (labels ((new-buffer ()
			 (let ((new-buffer-size (* (1+ (ceiling (/ (1- size) +buffer-alignment+))) +buffer-alignment+)))
			   
			   (setf index-buffer
				 (create-buffer-1 device new-buffer-size VK_BUFFER_USAGE_INDEX_BUFFER_BIT
						  :buffer-class 'index-buffer :allocator (allocator renderer)))
			   (new-memory)))
		       
		       (new-memory ()
			 (let ((buffer-memory
				(allocate-buffer-memory device index-buffer VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
							:allocator (allocator renderer))))

			   (setf (allocated-memory index-buffer) buffer-memory)
			 
			   (bind-buffer-memory device index-buffer buffer-memory)))

		       (free-buffer ()
			 (vkDestroyBuffer (h device) (h index-buffer) (h (allocator index-buffer)))

			 (when (allocated-memory index-buffer)
			   (free-memory)))

		       (free-memory ()
			 (vkFreeMemory (h device) (h (allocated-memory index-buffer)) (h (allocator (allocated-memory index-buffer))))))
		
		(if (null index-buffer)
		    (new-buffer)
		    (when (or (null (allocated-memory index-buffer))
			      (< (vk::size index-buffer) size))
		      (free-buffer)
		      (new-buffer)))
		
		(mmap-buffer index-buffer (array-pointer index-array) size)))))))))

(defmethod update-uniform-buffer ((renderer renderer-mixin) &key model-matrix view-matrix projection-matrix)
  (with-slots (uniform-buffer) renderer
    (let ((type (uniform-buffer-type renderer))
	  (p-stage (uniform-buffer-stage renderer)))
      (copy-matrix-to-foreign model-matrix (foreign-slot-pointer p-stage type 'model))
      (copy-matrix-to-foreign view-matrix (foreign-slot-pointer p-stage type 'view))
      (copy-matrix-to-foreign projection-matrix (foreign-slot-pointer p-stage type 'proj))
      (copy-matrix-to-foreign +clip-matrix+ (foreign-slot-pointer p-stage type 'clip))
      (copy-uniform-buffer-memory (default-logical-device renderer)
				  p-stage
				  (allocated-memory uniform-buffer)
				  (foreign-type-size type)))
    (values)))

(defun clamp-sf (number)
  (cond ((> 0.0f0 number least-negative-single-float) 0.0f0)
	((< number most-negative-single-float) most-negative-single-float)
	((< 0.0f0 number least-positive-single-float) 0.0f0)
	((> number most-positive-single-float) most-positive-single-float)
	(t (coerce number 'single-float))))

(defun copy-matrix-to-foreign (lisp-matrix p-matrix)
  (let ((array (marr lisp-matrix)))
          #+nil
    (sb-sys:with-pinned-objects (array)
      ;; 3d-matrices stores elements in row major, we need to put them in column-major
      (memcpy p-matrix (sb-sys:vector-sap array) (load-time-value (foreign-type-size '(:struct mat4)))))
    (loop for i from 0 below 4
       do (loop for j from 0 below 4
	     do (setf (mem-aref p-matrix :float (+ j (* i 4)))
		      (clamp-sf (aref array (+ i (* j 4)))))))
    (values)))

(defmethod render ((renderer draw-indexed-renderer-mixin)
		   command-buffer frame-index
		   model-matrix view-matrix projection-matrix
		   width height)
  (with-slots (draw-list) renderer
    (unless (zerop (array-fill-pointer (draw-list-index-array draw-list)))
      (initialize-buffers renderer frame-index)
      (let ((frame-resource (get-frame-resource renderer frame-index)))
	(cmd-bind-pipeline command-buffer (pipeline renderer) :bind-point :graphics)
	(update-uniform-buffer renderer
			       :model-matrix model-matrix
			       :view-matrix view-matrix
			       :projection-matrix projection-matrix)
	(cmd-set-viewport command-buffer :width width :height height)
	(cmd-set-scissor command-buffer :width width :height height)
	(cmd-bind-vertex-buffers command-buffer (list (vertex-buffer frame-resource)))
	(cmd-bind-descriptor-sets command-buffer (pipeline-layout renderer) (list (descriptor-set renderer)))
	(cmd-bind-index-buffer command-buffer (index-buffer frame-resource))
	(loop for cmd across (draw-commands (renderer-draw-list renderer))
	   when cmd
	   do (cmd-draw-indexed command-buffer cmd))))
    (values)))

(defmethod append-colored-triangle ((renderer triangle-list-renderer)
				    (point1 vec3)
				    (color1 array)
				    (point2 vec3)
				    (color2 array)
				    (point3 vec3)
				    (color3 array)
				    &aux (start-index 0))
  (with-slots (draw-list) renderer
    (with-slots (vertex-array index-array) draw-list
      (let ((vertex-offset (/ (array-fill-pointer (draw-list-vertex-array draw-list))
			      (vertex-float-size renderer)))
	    (first-index (array-fill-pointer (draw-list-index-array draw-list))))
	
	(fnv-push-extend (clamp-sf (vx point1)) vertex-array)
	(fnv-push-extend (clamp-sf (vy point1)) vertex-array)
	(fnv-push-extend (clamp-sf (vz point1)) vertex-array)
	(fnv-push-extend (aref color1 0) vertex-array)
	(fnv-push-extend (aref color1 1) vertex-array)
	(fnv-push-extend (aref color1 2) vertex-array)
	(fnv-push-extend (aref color1 3) vertex-array)
	(fnv-push-extend start-index index-array)
	
	(fnv-push-extend (clamp-sf (vx point2)) vertex-array)
	(fnv-push-extend (clamp-sf (vy point2)) vertex-array)
	(fnv-push-extend (clamp-sf (vz point2)) vertex-array)
	(fnv-push-extend (aref color2 0) vertex-array)
	(fnv-push-extend (aref color2 1) vertex-array)
	(fnv-push-extend (aref color2 2) vertex-array)
	(fnv-push-extend (aref color2 3) vertex-array)
	(fnv-push-extend (1+ start-index) index-array)

	(fnv-push-extend (clamp-sf (vx point3)) vertex-array)
	(fnv-push-extend (clamp-sf (vy point3)) vertex-array)
	(fnv-push-extend (clamp-sf (vz point3)) vertex-array)
	(fnv-push-extend (aref color3 0) vertex-array)
	(fnv-push-extend (aref color3 1) vertex-array)
	(fnv-push-extend (aref color3 2) vertex-array)
	(fnv-push-extend (aref color3 3) vertex-array)
	(fnv-push-extend (+ 2 start-index) index-array)

	(let ((draw-cmd (vk::make-draw-indexed-cmd
			 :index-count 3
			 :first-index first-index
			 :vertex-offset vertex-offset
			 :draw-list draw-list)))
	  (vector-push-extend draw-cmd (draw-commands draw-list))))
      
      t)))
