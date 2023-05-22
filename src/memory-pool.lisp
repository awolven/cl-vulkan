(in-package :vk)

(defclass memory-pool-mixin ()
  ((name)
   (lock :initform (bt:make-lock))
   (memory-allocation :reader allocation)
   (device :accessor memory-pool-device)
   (buffer :accessor memory-pool-buffer)
   (allocated :initform (make-hash-table))
   (miniscule-free :initform nil :accessor memory-pool-miniscule-free)
   (tiny-free :initform nil :accessor memory-pool-tiny-free)
   (small-free :initform nil :accessor memory-pool-small-free)
   (regular-free :initform nil :accessor memory-pool-regular-free)
   (medium-free :initform nil :accessor memory-pool-medium-free)
   (large-free :initform nil :accessor memory-pool-large-free)
   (very-large-free :initform nil :accessor memory-pool-very-large-free)
   (huge-free :initform nil :accessor memory-pool-huge-free)
   (gigantic-free :initform nil :accessor memory-pool-gigantic-free)
   (ginormous-free :initform nil :accessor memory-pool-ginormous-free)
   (obscene-free :initform nil :accessor memory-pool-obscene-free)
   (custom-free :initform nil :accessor memory-pool-custom-free)))

(defclass memory-pool (memory-pool-mixin) ())

(defmethod initialize-instance :before ((instance memory-pool-mixin) &rest initargs
                                        &key device
                                          (properties
                                           (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                                                   VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)))
  (declare (ignore initargs))
  (setf (memory-pool-device instance) device)
  (let* ((usage (logior VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
			VK_BUFFER_USAGE_INDEX_BUFFER_BIT
			VK_BUFFER_USAGE_STORAGE_BUFFER_BIT))
	 (test-buffer (create-buffer-1 device (aligned-size 1) usage)))
    (with-vk-struct (p-requirements VkMemoryRequirements)
      (vkGetBufferMemoryRequirements (h device) (h test-buffer) p-requirements)
      (let* ((memory-type-index (find-memory-type
				 (physical-device device)
				 (foreign-slot-value
				  p-requirements
				  '(:struct VkMemoryRequirements)
				  '%vk::memoryTypeBits)
				 properties))
	     (memory-type (elt (memory-types (physical-device device)) memory-type-index))
	     (memory-heap (elt (memory-heaps (physical-device device)) (memory-type-heap-index memory-type)))
	     (size (min (memory-heap-size memory-heap) 1158791168 #+NIL(max-memory-allocation-count (physical-device device)))))
	(vkDestroyBuffer (h device) (h test-buffer) (h (allocator test-buffer)))
	(let ((big-buffer (create-buffer-1 device size usage)))
	  (with-vk-struct (p-alloc-info VkMemoryAllocateInfo)	       
	    (with-foreign-slots ((%vk::allocationSize
				  %vk::memoryTypeIndex)
				 p-alloc-info
				 (:struct VkMemoryAllocateInfo))
	      (setf %vk::allocationSize size
		    %vk::memoryTypeIndex memory-type-index)
	      (with-foreign-object (p-buffer-memory 'VkDeviceMemory)
		(check-vk-result (vkAllocateMemory (h device) p-alloc-info (h (allocator device)) p-buffer-memory))
		(setf (slot-value instance 'memory-allocation)
		      (make-instance 'allocated-memory
				     :handle (mem-aref p-buffer-memory 'VkDeviceMemory)
				     :device device
				     :allocator (allocator device)
				     :alignment (foreign-slot-value
						 p-requirements
						 '(:struct VkMemoryRequirements)
						 '%vk::alignment)
				     :size size)))
	      (setf (allocated-memory big-buffer) (slot-value instance 'memory-allocation))
	      (bind-buffer-memory device big-buffer (slot-value instance 'memory-allocation))
	      (setf (memory-pool-buffer instance) big-buffer)))))))
  (values))

(defstruct memory-resource
  (memory-pool)
  (offset)
  (size))

(defstruct (memory-resource-miniscule
	     (:include memory-resource)))

(defstruct (memory-resource-tiny
	     (:include memory-resource)))

(defstruct (memory-resource-small
	     (:include memory-resource)))

(defstruct (memory-resource-regular
	     (:include memory-resource)))

(defstruct (memory-resource-medium
	     (:include memory-resource)))

(defstruct (memory-resource-large
	     (:include memory-resource)))

(defstruct (memory-resource-very-large
	     (:include memory-resource)))

(defstruct (memory-resource-huge
	     (:include memory-resource)))

(defstruct (memory-resource-gigantic
	     (:include memory-resource)))

(defstruct (memory-resource-ginormous
	     (:include memory-resource)))

(defstruct (memory-resource-obscene
	     (:include memory-resource)))

(defstruct (memory-resource-custom
	     (:include memory-resource))
  (buffer)
  (allocation))

(defconstant +buffer-alignment+ 256)

(defun aligned-size (size)
  (* (1+ (ceiling (/ (1- size) +buffer-alignment+))) +buffer-alignment+))

(defun %acquire-memory-custom-sized (memory-pool size &key (properties
							    (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
								    VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)))
  (let ((actual-size (aligned-size (round (* 2 size))))
	(device (memory-pool-device memory-pool)))
    (or (loop for memory-resource in (memory-pool-custom-free memory-pool)
	   when (>= actual-size (memory-resource-size memory-resource))
	   do (setf (memory-pool-custom-free memory-pool) (delete memory-resource (memory-pool-custom-free memory-pool)))
	     (return (setf (gethash memory-resource (slot-value memory-pool 'allocated)) memory-resource)))
	
	(let* ((usage (logior VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
			      VK_BUFFER_USAGE_INDEX_BUFFER_BIT
			      VK_BUFFER_USAGE_STORAGE_BUFFER_BIT))
	       (buffer (create-buffer-1 device actual-size usage)))
	  (with-vk-struct (p-requirements VkMemoryRequirements)
	    (vkGetBufferMemoryRequirements (h device) (h buffer) p-requirements)
	    (let* ((memory-type-index (find-memory-type
				       (physical-device device)
				       (foreign-slot-value
					p-requirements
					'(:struct VkMemoryRequirements)
					'%vk::memoryTypeBits)
				       properties)))
	      
	      (with-vk-struct (p-alloc-info VkMemoryAllocateInfo)
		(with-foreign-slots ((%vk::allocationSize
				      %vk::memoryTypeIndex)
				     p-alloc-info
				     (:struct VkMemoryAllocateInfo))
		  (setf %vk::allocationSize actual-size
			%vk::memoryTypeIndex memory-type-index)
		  
		  (with-foreign-object (p-buffer-memory 'VkDeviceMemory)
		    (check-vk-result (vkAllocateMemory (h device) p-alloc-info (h (allocator device)) p-buffer-memory))
		    (let ((allocation (make-instance 'allocated-memory
					 :handle (mem-aref p-buffer-memory 'VkDeviceMemory)
					 :device device
					 :allocator (allocator device)
					 :alignment (foreign-slot-value
						     p-requirements
						     '(:struct VkMemoryRequirements)
						     '%vk::alignment)
					 :size actual-size)))
		      (setf (allocated-memory buffer) allocation)
		      (bind-buffer-memory device buffer allocation)
		      (let ((memory-resource (make-memory-resource-custom
					      :buffer buffer
					      :allocation allocation
					      :memory-pool memory-pool
					      :offset 0
					      :size actual-size)))
			(setf (gethash memory-resource (slot-value memory-pool 'allocated)) memory-resource))))))))))))

(defparameter *miniscule-buffer-size* (expt 2 11)) ;; 2 kB  --> 4
(defparameter *tiny-buffer-size* (expt 2 13)) ;; 8 kB --> 6
(defparameter *small-buffer-size* (expt 2 15)) ;; 32 kB --> 0
(defparameter *regular-buffer-size* (expt 2 17)) ;; 128 kB --> 2
(defparameter *medium-buffer-size* (expt 2 19)) ;; 512 kB --> 0
(defparameter *large-buffer-size* (expt 2 21)) ;; 2 MB --> 2
(defparameter *very-large-buffer-size* (expt 2 23)) ;; 8 MB  --> 2
(defparameter *huge-buffer-size* (expt 2 25)) ;; 32 MB --> 29
(defparameter *gigantic-buffer-size* (expt 2 27)) ;; 128 MB --> 20
(defparameter *ginormous-buffer-size* (expt 2 29)) ;; 512 MB --> 11
(defparameter *obscene-buffer-size* (expt 2 31)) ;; 2 GB

(defun get-buffer-counts (max-size)
  (loop for i from 11 to 29 by 2
     for n in '(8 12 0 8 0 8 8 8 6)
     with sum = 0
     with next
     with collection = ()
     do (setq next (* n (expt 2 i)))
       (if (< (+ sum next) max-size)
	   (progn (setq sum (+ sum next))
		  (push n collection))
	   (return (values (nreverse collection) sum (- max-size sum))))
     finally (return (values (nreverse collection) sum (- max-size sum)))))

(defun initialize-buffer-memory-pool (dpy)
  (setf (memory-pool dpy)
        (make-instance 'memory-pool
                       :device (default-logical-device dpy)
                       :properties (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                                           VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))))

(defmethod initialize-instance :after ((instance memory-pool-mixin)
                                       &rest initargs)
  (declare (ignore initargs))
  (let ((buffer-counts (get-buffer-counts (allocated-memory-size (allocation instance)))))
    (loop for count in buffer-counts
       for i from 11 by 2
       for name in '("MINISCULE" "TINY" "SMALL" "REGULAR" "MEDIUM" "LARGE" "VERY-LARGE" "HUGE" "GIGANTIC" "GINORMOUS" "OBSCENE")
       do (loop for j from 0 below count
	     do (eval `(push (,(intern (concatenate 'string "MAKE-MEMORY-RESOURCE-" name) :vk)
			       :memory-pool ,instance
			       :offset ,(* j (expt 2 i))
			       :size ,(expt 2 i))
			     (,(intern (concatenate 'string "MEMORY-POOL-" name "-FREE") :vk) ,instance)))))
    (values)))

(defmacro define-memory-acquisition-function (name)
  `(defun ,(intern (concatenate 'string "%ACQUIRE-MEMORY-" name) :vk) (memory-pool)
     (declare (type memory-pool-mixin memory-pool))
     (bt:with-lock-held ((slot-value memory-pool 'lock))
       (when (,(intern (concatenate 'string "MEMORY-POOL-" name "-FREE")) memory-pool)
	 (let ((new (pop (,(intern (concatenate 'string "MEMORY-POOL-" name "-FREE") :vk) memory-pool))))
	   (when new
	     (setf (gethash new (slot-value memory-pool 'allocated)) new)))))))

(define-memory-acquisition-function "MINISCULE")
(define-memory-acquisition-function "TINY")
(define-memory-acquisition-function "SMALL")
(define-memory-acquisition-function "REGULAR")
(define-memory-acquisition-function "MEDIUM")
(define-memory-acquisition-function "LARGE")
(define-memory-acquisition-function "VERY-LARGE")
(define-memory-acquisition-function "HUGE")
(define-memory-acquisition-function "GIGANTIC")
(define-memory-acquisition-function "GINORMOUS")
(define-memory-acquisition-function "OBSCENE")

(defun acquire-vertex-memory-sized (dpy size properties)
  (acquire-memory-sized (memory-pool dpy) size properties))

(defun acquire-index-memory-sized (dpy size properties)
  (acquire-memory-sized (memory-pool dpy) size properties))

(defun acquire-storage-memory-sized (dpy size properties)
  (acquire-memory-sized (memory-pool dpy)  size properties))

(defun release-vertex-memory (dpy memory-resource)
  (release-memory-resource dpy memory-resource))

(defun release-index-memory (dpy memory-resource)
  (release-memory-resource dpy memory-resource))

(defun release-storage-memory (dpy memory-resource)
  (release-memory-resource dpy memory-resource))

(defun vertex-buffer-memory-pool (dpy)
  (memory-pool dpy))

(defun index-buffer-memory-pool (dpy)
  (memory-pool dpy))

(defun storage-buffer-memory-pool (dpy)
  (memory-pool dpy))

(defun acquire-memory-sized (memory-pool size properties)
  (declare (ignore properties))
  (when (not (plusp size))
    (error "invalid size argument in acquire-memory-sized"))
  (or (cond ((<= size *miniscule-buffer-size*)
	     (or (%acquire-memory-miniscule memory-pool)
		 (%acquire-memory-tiny memory-pool)
		 (%acquire-memory-small memory-pool)))
	    ((<= size *tiny-buffer-size*)
	     (or (%acquire-memory-tiny memory-pool)
		 (%acquire-memory-small memory-pool)
		 (%acquire-memory-regular memory-pool)))
	    ((<= size *small-buffer-size*)
	     (or (%acquire-memory-small memory-pool)
		 (%acquire-memory-regular memory-pool)
		 (%acquire-memory-medium memory-pool)))
	    ((<= size *regular-buffer-size*)
	     (or (%acquire-memory-regular memory-pool)
		 (%acquire-memory-medium memory-pool)
		 (%acquire-memory-large memory-pool)))
	    ((<= size *medium-buffer-size*)
	     (or (%acquire-memory-medium memory-pool)
		 (%acquire-memory-large memory-pool)
		 (%acquire-memory-very-large memory-pool)))
	    ((<= size *large-buffer-size*)
	     (or (%acquire-memory-large memory-pool)
		 (%acquire-memory-very-large memory-pool)
		 (%acquire-memory-huge memory-pool)))
	    ((<= size *very-large-buffer-size*)
	     (or (%acquire-memory-very-large memory-pool)
		 (%acquire-memory-huge memory-pool)
		 (%acquire-memory-gigantic memory-pool)))
	    ((<= size *huge-buffer-size*)
	     (or (%acquire-memory-huge memory-pool)
		 (%acquire-memory-gigantic memory-pool)
		 (%acquire-memory-ginormous memory-pool)))
	    ((<= size *gigantic-buffer-size*)
	     (or (%acquire-memory-gigantic memory-pool)
		 (%acquire-memory-ginormous memory-pool)
		 (%acquire-memory-obscene memory-pool)))
	    ((<= size *ginormous-buffer-size*)
	     (or (%acquire-memory-ginormous memory-pool)
		 (%acquire-memory-obscene memory-pool)))
	    ((<= size *obscene-buffer-size*)
	     (%acquire-memory-obscene memory-pool)))
      (%acquire-memory-custom-sized memory-pool size)))

(defun release-memory-resource (dpy memory-resource)
  (let ((memory-pool (memory-pool dpy)))
    ;; todo, put the free lists in an array in the memory-pool
    ;; and store the index in the memory resource
    ;; so we can just use an aref instead of a typecase.
    (when (typecase memory-resource
	    (memory-resource-miniscule
	     (push memory-resource (memory-pool-miniscule-free memory-pool))
	     t)
	    (memory-resource-tiny
	     (push memory-resource (memory-pool-tiny-free memory-pool))
	     t)
	    (memory-resource-small
	     (push memory-resource (memory-pool-small-free memory-pool))
	     t)
	    (memory-resource-regular
	     (push memory-resource (memory-pool-regular-free memory-pool))
	     t)
	    (memory-resource-medium
	     (push memory-resource (memory-pool-medium-free memory-pool))
	     t)
	    (memory-resource-large
	     (push memory-resource (memory-pool-large-free memory-pool))
	     t)	     
	    (memory-resource-very-large
	     (push memory-resource (memory-pool-very-large-free memory-pool))
	     t)
	    (memory-resource-huge
	     (push memory-resource (memory-pool-huge-free memory-pool))
	     t)
	    (memory-resource-gigantic
	     (push memory-resource (memory-pool-gigantic-free memory-pool))
	     t)
	    (memory-resource-ginormous
	     (push memory-resource (memory-pool-ginormous-free memory-pool))
	     t)
	    (memory-resource-obscene
	     (push memory-resource (memory-pool-obscene-free memory-pool))
	     t)
	    (memory-resource-custom
	     (push memory-resource (memory-pool-obscene-free memory-pool))
	     t)
	    (t (warn "~S is not a known memory-resource type" memory-resource)))
      (remhash memory-resource (slot-value memory-pool 'allocated))
      (values))))
      
    

(defun destroy-memory-pools (app)
  (let ((mp (memory-pool app)))
    (%vk:vkdestroybuffer (h (device (memory-pool-buffer mp)))
			 (h (memory-pool-buffer mp))
			 (h (allocator (memory-pool-buffer mp))))
    (%vk:vkfreememory (h (device (allocated-memory (memory-pool-buffer mp))))
		      (h (allocated-memory (memory-pool-buffer mp)))
		      (h (allocator (allocated-memory (memory-pool-buffer mp)))))
    (values)))
    



