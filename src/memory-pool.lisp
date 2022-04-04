(in-package :vk)

(defclass memory-pool-mixin ()
  ((name)
   (lock :initform (bt:make-lock))
   (memory-allocation :reader allocation)
   (allocated :initform (make-hash-table))
   (free :initform nil :accessor memory-pool-free)))


(defclass small-host-visible-host-coherent-memory-pool (memory-pool-mixin)
  ())

(defclass large-host-visible-host-coherent-memory-pool (memory-pool-mixin)
  ())

(defclass large-device-local-memory-pool (memory-pool-mixin)
  ())

(defmethod initialize-instance :before ((instance memory-pool-mixin) &rest initargs
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
                                                   :size size)))))))
  (values))

(defun initialize-small-host-visible-memory-pool (app)
  (setf (small-host-visible-memory-pool app)
        (make-instance 'small-host-visible-host-coherent-memory-pool
                       :size (expt 2 27)
                       :device (default-logical-device app)
                       :properties (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                                           VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))))

(defun initialize-large-host-visible-memory-pool (app)
  (setf (large-host-visible-memory-pool app)
        (make-instance 'large-host-visible-host-coherent-memory-pool
                       :size (expt 2 26)
                       :device (default-logical-device app)
                       :properties (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                                           VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))))

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

(defmethod initialize-instance :after ((instance small-host-visible-host-coherent-memory-pool)
                                       &rest initargs)
  (declare (ignore initargs))
  (loop for i from 0 below 16384
        do (push (make-memory-resource :memory-pool instance
                                     :offset (* i #.(expt 2 13))
                                     :size #.(expt 2 13))
                 (memory-pool-free instance)))
  (values))

(defmethod initialize-instance :after ((instance large-host-visible-host-coherent-memory-pool)
                                       &rest initargs)
  (declare (ignore initargs))
  (loop for i from 0 below 16384
        do (push (make-memory-resource :memory-pool instance
                                     :offset (* i #.(expt 2 17))
                                     :size #.(expt 2 17))
                 (memory-pool-free instance)))
  (values))

(defmethod initialize-instance :after ((instance large-device-local-memory-pool)
                                       &rest initargs)
  (declare (ignore initargs))
  (loop for i from 0 below 16384
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

(defun acquire-memory-sized (app size properties)
  (if (eq properties :device-local)
      (%acquire-memory (large-device-local-memory-pool app))
      (cond ((<= size #.(expt 2 13))
             (or (%acquire-memory (small-host-visible-memory-pool app))
                 (%acquire-memory (large-host-visible-memory-pool app))))
            ((<= size #.(expt 2 17))
             (%acquire-memory (large-host-visible-memory-pool app)))
            (t nil))))

(defun release-memory (app memory)
  (cond ((<= (memory-resource-size memory) #.(expt 2 13))
         (%release-memory (small-host-visible-memory-pool app) memory))
        ((<= (memory-resource-size memory) #.(expt 2 17))
         (if (eq (memory-resource-memory-pool memory) (large-device-local-memory-pool app))
             (%release-memory (large-device-local-memory-pool app) memory)
             (%release-memory (large-host-visible-memory-pool app) memory)))
        (t (error "Cannot release ~S" memory))))
