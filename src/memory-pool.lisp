(in-package :vk)

(defclass memory-pool-mixin ()
  ((name)
   (lock :initform (bt:make-lock))
   (memory-allocation :reader allocation)
   (allocated :initform (make-hash-table))
   (free :initform nil :accessor memory-pool-free)))

(defclass vertex-buffer-memory-pool-mixin (memory-pool-mixin)
  ((buffer :accessor memory-pool-vertex-buffer)))

(defclass index-buffer-memory-pool-mixin (memory-pool-mixin)
  ((buffer :accessor memory-pool-index-buffer)))


#+NIL(defclass small-host-visible-host-coherent-memory-pool (memory-pool-mixin)
  ())

#+NIL(defclass large-host-visible-host-coherent-memory-pool (memory-pool-mixin)
  ())

#+NIL(defclass large-device-local-memory-pool (memory-pool-mixin)
  ())

(defmethod initialize-instance :before ((instance vertex-buffer-memory-pool-mixin) &rest initargs
                                        &key device size
                                          (properties
                                           (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                                                   VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)))
  (declare (ignore initargs))
  (let ((test-buffer (create-buffer-1 device size VK_BUFFER_USAGE_VERTEX_BUFFER_BIT)))
    (with-vk-struct (p-requirements VkMemoryRequirements)
      (vkGetBufferMemoryRequirements (h device) (h test-buffer) p-requirements)
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
          (setf (allocated-memory test-buffer) (slot-value instance 'memory-allocation))
          (bind-buffer-memory device test-buffer (slot-value instance 'memory-allocation))
          (setf (memory-pool-vertex-buffer instance) test-buffer)))))
  (values))

(defmethod initialize-instance :before ((instance index-buffer-memory-pool-mixin) &rest initargs
                                        &key device size
                                          (properties
                                           (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                                                   VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)))
  (declare (ignore initargs))
  (let ((test-buffer (create-buffer-1 device size VK_BUFFER_USAGE_INDEX_BUFFER_BIT)))
    (with-vk-struct (p-requirements VkMemoryRequirements)
      (vkGetBufferMemoryRequirements (h device) (h test-buffer) p-requirements)
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
          (setf (allocated-memory test-buffer) (slot-value instance 'memory-allocation))
          (bind-buffer-memory device test-buffer (slot-value instance 'memory-allocation))
          (setf (memory-pool-index-buffer instance) test-buffer)))))
  (values))

(defun initialize-vertex-buffer-memory-pool (app)
  (setf (vertex-buffer-memory-pool app)
        (make-instance 'vertex-buffer-memory-pool-mixin
                       :size (expt 2 27)
                       :device (default-logical-device app)
                       :properties (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                                           VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))))

(defun initialize-index-buffer-memory-pool (app)
  (setf (index-buffer-memory-pool app)
        (make-instance 'index-buffer-memory-pool-mixin
                       :size (expt 2 27)
                       :device (default-logical-device app)
                       :properties (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                                           VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))))

#+NIL
(defun initialize-large-host-visible-memory-pool (app)
  (setf (large-host-visible-memory-pool app)
        (make-instance 'large-host-visible-host-coherent-memory-pool
                       :size (expt 2 26)
                       :device (default-logical-device app)
                       :properties (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                                           VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))))

#+NIL
(defun initialize-large-device-local-memory-pool (app)
  (setf (large-device-local-memory-pool app)
        (make-instance 'large-device-local-memory-pool
                       :size (expt 2 26)
                       :device (default-logical-device app)
                       :properties VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)))

(defstruct memory-resource
  (memory-pool)
  (offset)
  (size))

(defmethod initialize-instance :after ((instance vertex-buffer-memory-pool-mixin)
                                       &rest initargs)
  (declare (ignore initargs))
  (loop for i from 0 below 1024
        do (push (make-memory-resource :memory-pool instance
                                     :offset (* i #.(expt 2 17))
                                     :size #.(expt 2 17))
                 (memory-pool-free instance)))
  (values))

(defmethod initialize-instance :after ((instance index-buffer-memory-pool-mixin)
                                       &rest initargs)
  (declare (ignore initargs))
  (loop for i from 0 below 1024
        do (push (make-memory-resource :memory-pool instance
                                     :offset (* i #.(expt 2 17))
                                     :size #.(expt 2 17))
                 (memory-pool-free instance)))
  (values))



(defun %acquire-memory (memory-pool)
  (declare (type memory-pool-mixin memory-pool))
  (bt:with-lock-held ((slot-value memory-pool 'lock))
    (let ((free (pop (memory-pool-free memory-pool))))
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
            (push memory (memory-pool-free memory-pool))
            (values))
          (error "Memory was not acquired out: ~S" memory)))))

(defun acquire-vertex-memory-sized (app size properties)
  (if (eq properties :device-local)
      (%acquire-memory (vertex-buffer-memory-pool app))
      (cond ((<= size #.(expt 2 17))
             (or (%acquire-memory (vertex-buffer-memory-pool app))
                 (error "bar")
                 #+NIL
                 (%acquire-memory (large-host-visible-memory-pool app))))
            ((<= size #.(expt 2 17))
             (error "blah")
             #+NIL
             (%acquire-memory (large-host-visible-memory-pool app)))
            (t nil))))

(defun acquire-index-memory-sized (app size properties)
  (if (eq properties :device-local)
      (%acquire-memory (index-buffer-memory-pool app))
      (cond ((<= size #.(expt 2 17))
             (or (%acquire-memory (index-buffer-memory-pool app))
                 (error "foo")
                 #+NIL
                 (%acquire-memory (large-host-visible-memory-pool app))))
            ((<= size #.(expt 2 17))
             (error "baz")
             #+NIL
             (%acquire-memory (large-host-visible-memory-pool app)))
            (t nil))))

(defun vertex-release-memory (app memory)
  (cond ((<= (memory-resource-size memory) #.(expt 2 17))
         (%release-memory (vertex-buffer-memory-pool app) memory))

        ((<= (memory-resource-size memory) #.(expt 2 17))
         (error "yuck")
         #+NIL
         (if (eq (memory-resource-memory-pool memory) (large-device-local-memory-pool app))
             (%release-memory (large-device-local-memory-pool app) memory)
             (%release-memory (large-host-visible-memory-pool app) memory)))
        (t (error "Cannot release ~S" memory))))

(defun index-release-memory (app memory)
  (cond ((<= (memory-resource-size memory) #.(expt 2 17))
         (%release-memory (index-buffer-memory-pool app) memory))
        ((<= (memory-resource-size memory) #.(expt 2 17))
         (error "yugg")
         #+NIL
         (if (eq (memory-resource-memory-pool memory) (large-device-local-memory-pool app))
             (%release-memory (large-device-local-memory-pool app) memory)
             (%release-memory (large-host-visible-memory-pool app) memory)))
        (t (error "Cannot release ~S" memory))))
