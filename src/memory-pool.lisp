(in-package :vk)

(defclass memory-pool-mixin ()
  ((name)
   (lock :initform (bt:make-lock))
   (memory-allocation :reader allocation)
   (allocated :initform (make-hash-table))
   (small-free :initform nil :accessor memory-pool-small-free)
   (medium-free :initform nil :accessor memory-pool-medium-free)
   (large-free :initform nil :accessor memory-pool-large-free)
   (buffer :accessor memory-pool-buffer)))

(defclass vertex-buffer-memory-pool (memory-pool-mixin) ())

(defclass index-buffer-memory-pool (memory-pool-mixin) ())

(defclass storage-buffer-memory-pool (memory-pool-mixin) ())

(defmethod memory-pool-buffer-usage ((memory-pool vertex-buffer-memory-pool))
  VK_BUFFER_USAGE_VERTEX_BUFFER_BIT)

(defmethod memory-pool-buffer-usage ((memory-pool index-buffer-memory-pool))
  VK_BUFFER_USAGE_INDEX_BUFFER_BIT)

(defmethod memory-pool-buffer-usage ((memory-pool storage-buffer-memory-pool))
  VK_BUFFER_USAGE_STORAGE_BUFFER_BIT)

(defmethod initialize-instance :before ((instance memory-pool-mixin) &rest initargs
                                        &key device size
                                          (properties
                                           (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                                                   VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)))
  (declare (ignore initargs))
  (let ((big-buffer (create-buffer-1 device size (memory-pool-buffer-usage instance))))
    (with-vk-struct (p-requirements VkMemoryRequirements)
      (vkGetBufferMemoryRequirements (h device) (h big-buffer) p-requirements)
      (with-vk-struct
          (p-alloc-info VkMemoryAllocateInfo)
        (with-foreign-slots ((%vk::allocationSize
                              %vk::memoryTypeIndex)
                             p-alloc-info
                             (:struct VkMemoryAllocateInfo))
          (setf %vk::allocationSize size
                %vk::memoryTypeIndex (find-memory-type
                                      (physical-device device)
                                      (foreign-slot-value
                                       p-requirements
                                       '(:struct VkMemoryRequirements)
                                       '%vk::memoryTypeBits)
                                      properties))
          (with-foreign-object (p-buffer-memory 'VkDeviceMemory)
            (check-vk-result (vkAllocateMemory (h device) p-alloc-info (h (allocator device)) p-buffer-memory))
            (setf (slot-value instance 'memory-allocation)
                  (make-instance 'allocated-memory :handle (mem-aref p-buffer-memory 'VkDeviceMemory)
                                                   :device device
                                                   :allocator (allocator device)
                                                   :alignment (foreign-slot-value
                                                               p-requirements
                                                               '(:struct VkMemoryRequirements)
                                                               '%vk::alignment)
                                                   :size size)))
          (setf (allocated-memory big-buffer) (slot-value instance 'memory-allocation))
          (bind-buffer-memory device big-buffer (slot-value instance 'memory-allocation))
          (setf (memory-pool-buffer instance) big-buffer)))))
  (values))

(defstruct memory-resource
  (memory-pool)
  (offset)
  (size))

(defstruct (memory-resource-small
	    (:include memory-resource)))

(defstruct (memory-resource-medium
	    (:include memory-resource)))

(defstruct (memory-resource-large
	    (:include memory-resource)))

(defparameter *memory-pool-small-buffer-size* (expt 2 17))
(defparameter *memory-pool-small-buffer-count* 1024)
(defparameter *memory-pool-medium-buffer-size* (expt 2 19))
(defparameter *memory-pool-medium-buffer-count* 512)
(defparameter *memory-pool-large-buffer-size* (expt 2 21))
(defparameter *memory-pool-large-buffer-count* 64)
;; with these current numbers, for an index buffer and a vertex buffer
;; the system tries to allocate 1G from the video card for the memory pools
(defparameter *memory-pool-size*
  (+ (* *memory-pool-small-buffer-count* *memory-pool-small-buffer-size*)
     (* *memory-pool-medium-buffer-count* *memory-pool-medium-buffer-size*)
     (* *memory-pool-large-buffer-count* *memory-pool-large-buffer-size*)))

(defun initialize-vertex-buffer-memory-pool (app)
  (setf (vertex-buffer-memory-pool app)
        (make-instance 'vertex-buffer-memory-pool
                       :size *memory-pool-size*
                       :device (default-logical-device app)
                       :properties (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                                           VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))))

(defun initialize-index-buffer-memory-pool (app)
  (setf (index-buffer-memory-pool app)
        (make-instance 'index-buffer-memory-pool
                       :size *memory-pool-size*
                       :device (default-logical-device app)
                       :properties (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                                           VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))))

(defun initialize-storage-buffer-memory-pool (app)
  (setf (storage-buffer-memory-pool app)
        (make-instance 'storage-buffer-memory-pool
                       :size *memory-pool-size*
                       :device (default-logical-device app)
                       :properties (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                                           VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))))

(defmethod initialize-instance :after ((instance memory-pool-mixin)
                                       &rest initargs)
  (declare (ignore initargs))
  (loop for i from 0 below *memory-pool-small-buffer-count*
        do (push (make-memory-resource-small
		  :memory-pool instance
		  :offset (* i *memory-pool-small-buffer-size*)
		  :size *memory-pool-small-buffer-size*)
                 (memory-pool-small-free instance)))
  (loop for i from 0 below *memory-pool-medium-buffer-count*
        do (push (make-memory-resource-medium
		  :memory-pool instance
		  :offset (+ (* i *memory-pool-medium-buffer-size*)
			     (* *memory-pool-small-buffer-count*
				*memory-pool-small-buffer-size*))
		  :size *memory-pool-medium-buffer-size*)
                 (memory-pool-medium-free instance)))
  (loop for i from 0 below *memory-pool-large-buffer-count*
        do (push (make-memory-resource-large
		  :memory-pool instance
		  :offset (+ (* i *memory-pool-large-buffer-size*)
			     (* *memory-pool-small-buffer-count*
				*memory-pool-small-buffer-size*)
			     (* *memory-pool-medium-buffer-count*
				*memory-pool-medium-buffer-size*))
		  :size *memory-pool-large-buffer-size*)
                 (memory-pool-large-free instance)))
  (values))

(defun %acquire-memory-small (memory-pool)
  (declare (type memory-pool-mixin memory-pool))
  (bt:with-lock-held ((slot-value memory-pool 'lock))
    (let ((free (pop (memory-pool-small-free memory-pool))))
      (when free
        (setf (gethash free (slot-value memory-pool 'allocated)) free)))))

(defun %acquire-memory-medium (memory-pool)
  (declare (type memory-pool-mixin memory-pool))
  (bt:with-lock-held ((slot-value memory-pool 'lock))
    (let ((free (pop (memory-pool-medium-free memory-pool))))
      (when free
        (setf (gethash free (slot-value memory-pool 'allocated)) free)))))

(defun %acquire-memory-large (memory-pool)
  (declare (type memory-pool-mixin memory-pool))
  (bt:with-lock-held ((slot-value memory-pool 'lock))
    (let ((free (pop (memory-pool-large-free memory-pool))))
      (when free
        (setf (gethash free (slot-value memory-pool 'allocated)) free)))))

(defun %release-memory (memory-pool memory)
  (declare (type memory-pool-mixin memory-pool))
  (declare (type memory-resource memory))
  (bt:with-lock-held ((slot-value memory-pool 'lock))
    (let ((result (gethash memory (slot-value memory-pool 'allocated))))
      (if result
          (progn
            (remhash memory (slot-value memory-pool 'allocated))
	    (etypecase memory
	      (memory-resource-small (push memory (memory-pool-small-free memory-pool)))
	      (memory-resource-medium (push memory (memory-pool-medium-free memory-pool)))
	      (memory-resource-large (push memory (memory-pool-large-free memory-pool))))
            (values))
          (error "Memory was not acquired out: ~S" memory)))))

(defun %acquire-memory-sized (memory-pool size properties)
  (declare (ignore properties))
  (let ((free
	  (cond ((<= size *memory-pool-small-buffer-size*)
		 (or (%acquire-memory-small memory-pool)
		     (%acquire-memory-medium memory-pool)
		     (%acquire-memory-large memory-pool)))
		((<= size *memory-pool-medium-buffer-size*)
		 (or (%acquire-memory-medium memory-pool)
		     (%acquire-memory-large memory-pool)))
		((<= size *memory-pool-large-buffer-size*)
		 (%acquire-memory-large memory-pool)))))
    (unless free (error "could not get memory."))
    free))
    
(defun acquire-vertex-memory-sized (app size properties)
  (%acquire-memory-sized (vertex-buffer-memory-pool app) size properties))

(defun acquire-index-memory-sized (app size properties)
  (%acquire-memory-sized (index-buffer-memory-pool app) size properties))

(defun acquire-storage-memory-sized (app size properties)
  (%acquire-memory-sized (storage-buffer-memory-pool app) size properties))

(defun release-vertex-memory (app memory-resource)
  (%release-memory (vertex-buffer-memory-pool app) memory-resource))

(defun release-index-memory (app memory-resource)
  (%release-memory (index-buffer-memory-pool app) memory-resource))

(defun release-storage-memory (app memory-resource)
  (%release-memory (storage-buffer-memory-pool app) memory-resource))


(defun destroy-memory-pools (app)
  (let ((vmp (vertex-buffer-memory-pool app))
	(imp (index-buffer-memory-pool app)))
    (%vk:vkdestroybuffer (h (device (memory-pool-buffer vmp)))
			 (h (memory-pool-buffer vmp))
			 (h (allocator (memory-pool-buffer vmp))))
    (%vk:vkfreememory (h (device (allocated-memory (memory-pool-buffer vmp))))
		      (h (allocated-memory (memory-pool-buffer vmp)))
		      (h (allocator (allocated-memory (memory-pool-buffer vmp)))))
    (%vk:vkdestroybuffer (h (device (memory-pool-buffer imp)))
			 (h (memory-pool-buffer imp))
			 (h (allocator (memory-pool-buffer imp))))
    (%vk:vkfreememory (h (device (allocated-memory (memory-pool-buffer imp))))
		      (h (allocated-memory (memory-pool-buffer imp)))
		      (h (allocator (allocated-memory (memory-pool-buffer imp)))))
    (values)))
    



