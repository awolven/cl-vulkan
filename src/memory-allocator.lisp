(in-package :vk)

(defvar *alignment-size* 1024)

(defun aligned-size (size)
  (if (zerop (mod size *alignment-size*))
      size
      (* (1+ (ceiling (/ (1- size) *alignment-size*))) *alignment-size*)))

(defstruct memory-block
  (allocator)
  (buffer)
  (offset)
  (size)
  (next nil)
  (prev nil)
  (used? nil))

(defclass memory-allocator-mixin ()
  ((name)
   (lock :initform (bt:make-lock)
	 :reader memory-allocator-lock)
   (root-allocation :initarg :root-allocation
		    :reader allocation)
   (properties :initarg :properties :reader memory-properties)
   (device :accessor memory-allocator-device)
   (buffer :accessor memory-allocator-big-buffer)
   (allocated :initform (make-hash-table)
	      :reader memory-allocator-allocated)
   (miniscule-free :initform (list nil)
		   :reader memory-allocator-miniscule-free)
   (tiny-free :initform (list nil)
	      :reader memory-allocator-tiny-free)
   (small-free :initform (list nil)
	       :reader memory-allocator-small-free)
   (ordinary-free :initform (list nil)
		  :reader memory-allocator-ordinary-free)
   (medium-free :initform (list nil)
		:reader memory-allocator-medium-free)
   (large-free :initform (list nil)
	       :reader memory-allocator-large-free)
   (very-large-free :initform (list nil)
		    :reader memory-allocator-very-large-free)
   (huge-free :initform (list nil)
	      :reader memory-allocator-huge-free)
   (gigantic-free :initform (list nil)
		  :reader memory-allocator-gigantic-free)
   (ginormous-free :initform (list nil)
		   :reader memory-allocator-ginormous-free)
   (outlandish-free :initform (list nil)
		    :reader memory-allocator-outlandish-free)
   (obscene-free :initform (list nil)
		 :reader memory-allocator-obscene-free)
   (next :initform nil
	 :accessor memory-allocator-next)
   (prev :initform nil
	 :accessor memory-allocator-prev)))
  

(defclass memory-allocator (memory-allocator-mixin) ())

(defconstant VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT #x00020000)
(defconstant VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT #x00000002)

(defun try-allocation (device max-size min-size properties)
  (let* ((usage (logior VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
			VK_BUFFER_USAGE_INDEX_BUFFER_BIT
			VK_BUFFER_USAGE_STORAGE_BUFFER_BIT
			VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
			VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
			VK_BUFFER_USAGE_TRANSFER_SRC_BIT
			VK_BUFFER_USAGE_TRANSFER_DST_BIT))
	 (test-buffer (create-buffer-1 device (aligned-size 1) usage)))
    (let ((memory-type-index
	    (with-vk-struct (p-requirements VkMemoryRequirements)
	      (vkGetBufferMemoryRequirements (h device) (h test-buffer) p-requirements)
	      (find-memory-type
	       (physical-device device)
	       (foreign-slot-value
		p-requirements
		'(:struct VkMemoryRequirements)
		'%vk::memoryTypeBits)
	       properties))))
      (with-vk-struct (p-requirements VkMemoryRequirements)
	(vkGetBufferMemoryRequirements (h device) (h test-buffer) p-requirements)
	#+NIL
	(setf *alignment-size*
	      (foreign-slot-value p-requirements 
				  '(:struct VkMemoryRequirements) '%vk::alignment))
	(destroy-buffer-1 test-buffer)
	(with-vk-struct (p-alloc-flags-info VkMemoryAllocateFlagsInfo)
	  (with-foreign-slots ((%vk::flags)
			       p-alloc-flags-info
			       (:struct VkMemoryAllocateFlagsInfo))
	    (setf %vk::flags VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT))
	  (with-vk-struct (p-alloc-info VkMemoryAllocateInfo)
	    (with-foreign-slots ((%vk::allocationSize
				  %vk::memoryTypeIndex
				  %vk::pNext)
				 p-alloc-info
				 (:struct VkMemoryAllocateInfo))
	      (setf %vk::memoryTypeIndex memory-type-index
		    %vk::pNext p-alloc-flags-info)
	      (with-foreign-object (p-buffer-memory 'VkDeviceMemory)
		(let ((actual-size nil))
		  (let ((trials (mapcan
				       #'(lambda (size)
					   (if (and max-size
						    (> size max-size))
					       (if (and min-size
							(< size min-size))
						   nil
						   (list size))
					       (list size)))
				       '#.(loop for i from 32 downto 11
						collect (- (expt 2 i) 1024)))))
			  (when max-size
			    (pushnew max-size trials))
			  (when min-size
			    (setq trials (append trials (list min-size))))
			  (loop for size in trials
				with result
				do (setf %vk::allocationSize size)
				   (setq result
					 (vkAllocateMemory (h device)
							   p-alloc-info
							   (h (allocator device))
							   p-buffer-memory))
				when (= VK_SUCCESS result)
				  do (setq actual-size size)
				     (return (values (mem-aref p-buffer-memory 'VkDeviceMemory)
						     usage
						     actual-size
						     (foreign-slot-value
						      p-requirements
						      '(:struct VkMemoryRequirements)
						      '%vk::alignment)))
				unless (= VK_ERROR_OUT_OF_DEVICE_MEMORY result)
				  do (check-vk-result result)
				finally (check-vk-result result))))))))))))
  

(defmethod initialize-instance :before ((instance memory-allocator-mixin) &rest initargs
                                        &key device
					  memory usage actual-size alignment
					  max-size
					  (min-size *alignment-size*)
					  properties)
  (declare (ignore initargs))
  
  (setf (memory-allocator-device instance) device)
  (let ((allocation
	  (setf (slot-value instance 'root-allocation)
		(if memory
		    (make-instance 'allocated-memory
				      :handle memory
				      :device device
				      :allocator (allocator device)
				      :alignment alignment
				      :size actual-size)
		    
		    (multiple-value-bind (memory u a alignment)
			(try-allocation device max-size min-size properties)
		      (setq usage u)
		      (setq actual-size a)
		      (make-instance 'allocated-memory
				     :handle memory
				     :device device
				     :allocator (allocator device)
				     :alignment alignment
				     :size actual-size))))))
    (let ((big-buffer (create-buffer-1 device actual-size usage)))
      (setf (allocated-memory big-buffer) allocation)
      (bind-buffer-memory device big-buffer allocation)
      (setf (memory-allocator-big-buffer instance) big-buffer)))
  (values))
  
(defun get-byte-counts (allocator)
  (let* ((total (slot-value (allocation allocator) 'vk::size))
	 (used (let ((sum 0))
		 (maphash #'(lambda (k v)
			      (declare (ignore k))
			      (incf sum (memory-block-size v)))
			  (memory-allocator-allocated allocator))
		 sum))
	 (free (- total used)))
    (values total used free)))

(defmethod print-object ((object memory-allocator) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (multiple-value-bind (total used) (get-byte-counts object)
      (format stream " total: ~:d used: ~:d" total used))))


	  


(defconstant +miniscule-allocation+ #.(expt 2 11)) ;; >= alignment size and <= 2kb
(defconstant +tiny-allocation+ #.(expt 2 13)) ;; > 2kb and <= 8kb
(defconstant +small-allocation+ #.(expt 2 15)) ;; > 8kb and <= 32kb
(defconstant +ordinary-allocation+ #.(expt 2 17)) ;; > 32kb and <= 128kb
(defconstant +medium-allocation+ #.(expt 2 19)) ;; > 128kb and <= 512kb
(defconstant +large-allocation+ #.(expt 2 21)) ;; > 512kb and <= 2MB
(defconstant +very-large-allocation+ #.(expt 2 23)) ;; > 2MB and <= 8MB
(defconstant +huge-allocation+ #.(expt 2 25)) ;; > 8MB and <= 32MB
(defconstant +gigantic-allocation+ #.(expt 2 27)) ;; > 32MB and <= 128MB
(defconstant +ginormous-allocation+ #.(expt 2 29)) ;; > 128MB and <= 512MB
(defconstant +outlandish-allocation+ #.(expt 2 31)) ;; > 512MB and <= 2GB
;; obscene-allocation > 2GB

(defun acquire-memory-sized (device size properties)
  (when (not (plusp size))
    (error "invalid size argument in acquire-memory-sized"))
  (labels ((acquire (memory-allocator)
	     (cond ((<= size +miniscule-allocation+)
		    (%acquire-memory-miniscule memory-allocator size))
		   ((<= size +tiny-allocation+)
		    (%acquire-memory-tiny memory-allocator size))
		   ((<= size +small-allocation+)
		    (%acquire-memory-small memory-allocator size))
		   ((<= size +ordinary-allocation+)
		    (%acquire-memory-ordinary memory-allocator size))
		   ((<= size +medium-allocation+)
		    (%acquire-memory-medium memory-allocator size))
		   ((<= size +large-allocation+)
		    (%acquire-memory-large memory-allocator size))
		   ((<= size +very-large-allocation+)
		    (%acquire-memory-very-large memory-allocator size))
		   ((<= size +huge-allocation+)
		    (%acquire-memory-huge memory-allocator size))
		   ((<= size +gigantic-allocation+)
		    (%acquire-memory-gigantic memory-allocator size))
		   ((<= size +ginormous-allocation+)
		    (%acquire-memory-ginormous memory-allocator size))
		   ((<= size +outlandish-allocation+)
		    (%acquire-memory-outlandish memory-allocator size))
		   (t (%acquire-memory-obscene memory-allocator size))))

	   (maybe-make-allocator (properties)
	     (multiple-value-bind (memory usage actual-size alignment)
		 (handler-case
		     (try-allocation device nil size properties)
		   (error () nil))
	       (when memory
		 (make-instance 'memory-allocator
				:device device
				:memory memory
				:usage usage
				:actual-size actual-size
				:alignment alignment
				:properties properties))))
	   
	   (search-allocators (allocators)
	     (loop for alctr in (cdr allocators)
		   with allocation = nil
		   when (logtest properties (memory-properties alctr))
		     do (setq allocation (acquire alctr))
		   when allocation
		     do (return allocation)
		   finally (return (let ((new-alctr
					   (if (logtest VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT properties)
					       (maybe-make-allocator properties)
					       (or (maybe-make-allocator
						    (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
							    VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
							    VK_MEMORY_PROPERTY_HOST_CACHED_BIT
							    properties))
						   (maybe-make-allocator
						    (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
							    VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
							    properties))
						   (maybe-make-allocator
						    (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
							    properties))
						   (maybe-make-allocator properties)))))
				     (when new-alctr
				       (push new-alctr (cdr allocators))
				       (acquire new-alctr)))))))

    (if (logtest VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT properties)

	(or (let ((dlas (device-local-allocators device)))
	      (and dlas (search-allocators dlas)))
	    (let ((ndlas (non-device-local-allocators device)))
	      (and ndlas (search-allocators ndlas)))
	    (error "failed to allocate ~S" size))
	
	(or (let ((ndlas (non-device-local-allocators device)))
	      (and ndlas (search-allocators ndlas)))
	    (let ((dlas (device-local-allocators device)))
	      (and dlas (search-allocators dlas)))
	    (error "failed to allocate ~S" size)))))
				  


(defun search-for-block (allocator list size)
  (let ((aligned-size (aligned-size size)))
    (loop for head on list
	  with block = nil
	  with block-size
	  do (setq block (cadr head))
	  unless block
	    do (return nil)
	  do (setq block-size (memory-block-size block))
	  when (>= block-size aligned-size)
	    do (bt:with-lock-held ((memory-allocator-lock allocator))
		 (setf (cdr head) (cddr head))
		 (setf (memory-block-used? block) t)
		 (when (> block-size aligned-size)
		   (split-block block aligned-size))
		 (return
		   (setf (gethash block (memory-allocator-allocated allocator))
			 block))))))

(defun split-block (block size)
  (let* ((next (memory-block-next block))
	 (new-block (make-memory-block :allocator (memory-block-allocator block)
				       :buffer (memory-block-buffer block)
				       :offset (+ (memory-block-offset block) size)
				       :size (- (memory-block-size block) size)
				       :next next
				       :prev block
				       :used? nil)))
    (when next
      (setf (memory-block-prev next) new-block))
    (setf (memory-block-next block) new-block)
    (setf (memory-block-size block) size)
    (add-resulting-memory-block-to-free-list new-block)
    (values new-block block)))

(defmethod print-object ((object memory-block) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((size (memory-block-size object)))
      (cond ((<= size +miniscule-allocation+)
	     (princ "miniscule" stream))
	    ((<= size +tiny-allocation+)
	     (princ "tiny" stream))
	    ((<= size +small-allocation+)
	     (princ "small" stream))
	    ((<= size +ordinary-allocation+)
	     (princ "ordinary" stream))
	    ((<= size +medium-allocation+)
	     (princ "medium" stream))
	    ((<= size +large-allocation+)
	     (princ "large" stream))
	    ((<= size +very-large-allocation+)
	     (princ "very-large" stream))
	    ((<= size +huge-allocation+)
	     (princ "huge" stream))
	    ((<= size +gigantic-allocation+)
	     (princ "gigantic" stream))
	    ((<= size +ginormous-allocation+)
	     (princ "ginormous" stream))
	    ((<= size +outlandish-allocation+)
	     (princ "outlandish" stream))
	    (t (princ "obscene" stream))))
    (format stream " ~:d" (memory-block-size object))
    (when (memory-block-used? object)
      (princ " used" stream))
    object))

(defun add-resulting-memory-block-to-free-list (memory-block)
  (let ((size (memory-block-size memory-block))
	(allocator (memory-block-allocator memory-block)))
    (cond ((<= size +miniscule-allocation+)
	   (push memory-block (cdr (memory-allocator-miniscule-free allocator))))
	  ((<= size +tiny-allocation+)
	   (push memory-block (cdr (memory-allocator-tiny-free allocator))))
	  ((<= size +small-allocation+)
	   (push memory-block (cdr (memory-allocator-small-free allocator))))
	  ((<= size +ordinary-allocation+)
	   (push memory-block (cdr (memory-allocator-ordinary-free allocator))))
	  ((<= size +medium-allocation+)
	   (push memory-block (cdr (memory-allocator-medium-free allocator))))
	  ((<= size +large-allocation+)
	   (push memory-block (cdr (memory-allocator-large-free allocator))))
	  ((<= size +very-large-allocation+)
	   (push memory-block (cdr (memory-allocator-very-large-free allocator))))
	  ((<= size +huge-allocation+)
	   (push memory-block (cdr (memory-allocator-huge-free allocator))))
	  ((<= size +gigantic-allocation+)
	   (push memory-block (cdr (memory-allocator-gigantic-free allocator))))
	  ((<= size +ginormous-allocation+)
	   (push memory-block (cdr (memory-allocator-ginormous-free allocator))))
	  ((<= size +outlandish-allocation+)
	   (push memory-block (cdr (memory-allocator-outlandish-free allocator))))
	  (t (push memory-block (cdr (memory-allocator-obscene-free allocator)))))))

(defun remove-memory-block-from-free-list (old-size memory-block)
  (let ((allocator (memory-block-allocator memory-block)))
    (cond ((<= old-size +miniscule-allocation+)
	   (setf (cdr (memory-allocator-miniscule-free allocator))
		 (delete memory-block
			 (cdr (memory-allocator-miniscule-free allocator)))))
	  ((<= old-size +tiny-allocation+)
	   (setf (cdr (memory-allocator-tiny-free allocator))
		 (delete memory-block
			 (cdr (memory-allocator-tiny-free allocator)))))
	  ((<= old-size +small-allocation+)
	   (setf (cdr (memory-allocator-small-free allocator))
		 (delete memory-block
			 (cdr (memory-allocator-small-free allocator)))))
	  ((<= old-size +ordinary-allocation+)
	   (setf (cdr (memory-allocator-ordinary-free allocator))
		 (delete memory-block
			 (cdr (memory-allocator-ordinary-free allocator)))))
	  ((<= old-size +medium-allocation+)
	   (setf (cdr (memory-allocator-medium-free allocator))
		 (delete memory-block
			 (cdr (memory-allocator-medium-free allocator)))))
	  ((<= old-size +large-allocation+)
	   (setf (cdr (memory-allocator-large-free allocator))
		 (delete memory-block
			 (cdr (memory-allocator-large-free allocator)))))
	  ((<= old-size +very-large-allocation+)
	   (setf (cdr (memory-allocator-very-large-free allocator))
		 (delete memory-block
			 (cdr (memory-allocator-very-large-free allocator)))))
	  ((<= old-size +huge-allocation+)
	   (setf (cdr (memory-allocator-huge-free allocator))
		 (delete memory-block
			 (cdr (memory-allocator-huge-free allocator)))))
	  ((<= old-size +gigantic-allocation+)
	   (setf (cdr (memory-allocator-gigantic-free allocator))
		 (delete memory-block
			 (cdr (memory-allocator-gigantic-free allocator)))))
	  ((<= old-size +ginormous-allocation+)
	   (setf (cdr (memory-allocator-ginormous-free allocator))
		 (delete memory-block
			 (cdr (memory-allocator-ginormous-free allocator)))))
	  ((<= old-size +outlandish-allocation+)
	   (setf (cdr (memory-allocator-outlandish-free allocator))
		 (delete memory-block
			 (cdr (memory-allocator-outlandish-free allocator)))))
	  (t (setf (cdr (memory-allocator-obscene-free allocator))
		   (delete memory-block
			   (cdr (memory-allocator-obscene-free allocator))))))))

(defun release-memory (memory-block)
  (let* ((allocator (memory-block-allocator memory-block))
	 (allocated (memory-allocator-allocated allocator)))
    (if (null (gethash memory-block allocated))
	(error "memory block ~S was not allocated" memory-block)
	(progn
	  (bt:with-lock-held ((memory-allocator-lock allocator))
	    (remhash memory-block allocated)
	    (setf (memory-block-used? memory-block) nil)
	    
	    ;; coalesce:
	    (let ((prev (memory-block-prev memory-block)))
	      (when prev
		(unless (memory-block-used? prev)
		  (let ((prev-size (memory-block-size prev)))
		    (remove-memory-block-from-free-list prev-size prev)
		    (incf (memory-block-size prev) (memory-block-size memory-block))
		    (let ((next (memory-block-next memory-block)))
		      (setf (memory-block-next prev) next)
		      (when next
			(setf (memory-block-prev next) prev)))
		    (setq memory-block prev)))))
    
	    (let ((next (memory-block-next memory-block)))
	      (when next
		(unless (memory-block-used? next)
		  (let ((next-size (memory-block-size next)))
		    (remove-memory-block-from-free-list next-size next)
		    (incf (memory-block-size memory-block) next-size)
		    (let ((next-next (memory-block-next next)))
		      (setf (memory-block-next memory-block) next-next)
		      (when next-next
			(setf (memory-block-prev next-next) memory-block)))
		    ))))
	 
	    (add-resulting-memory-block-to-free-list memory-block)
	    (values))))))

(defparameter *miniscule-pool-size* 1536)
(defparameter *tiny-pool-size* 128)
(defparameter *small-pool-size* 512)
(defparameter *ordinary-pool-size* 8)
(defparameter *medium-pool-size* 0)
(defparameter *large-pool-size* 8)
(defparameter *very-large-pool-size* 6)
(defparameter *huge-pool-size* 4)
(defparameter *gigantic-pool-size* 3)
(defparameter *ginormous-pool-size* 0)
(defparameter *outlandish-pool-size* 0)
(defparameter *obscene-pool-size* 0)




(defun make-initial-block (allocator)
  (let* ((device (memory-allocator-device allocator))
	 (usage (logior VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
			VK_BUFFER_USAGE_INDEX_BUFFER_BIT
			VK_BUFFER_USAGE_STORAGE_BUFFER_BIT
			VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
			VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT))
	 (test-buffer (create-buffer-1 device (aligned-size 1) usage)))
    (with-vk-struct (p-requirements VkMemoryRequirements)
      (vkGetBufferMemoryRequirements (h device) (h test-buffer) p-requirements)
      (let ((memory-block (make-memory-block
			   :allocator allocator
			   :buffer (memory-allocator-big-buffer allocator)
			   :offset 0
			   :size (allocated-memory-size
				  (allocation allocator))
			   :next nil
			   :prev nil
			   :used? nil)))
	(add-resulting-memory-block-to-free-list memory-block)
	memory-block))))
	
    
(defmethod initialize-instance :after ((instance memory-allocator-mixin)
                                       &rest initargs)
  (declare (ignore initargs))
  (make-initial-block instance)
  (values))

(defun initialize-memory-allocators (device)
  (let ((found-indices ()))
    (mapcar
     #'(lambda (heap-type)
	 (let ((existing (assoc (memory-type-heap-index heap-type) found-indices)))
	   (if existing
	       (setf (cdr existing)
		     (max (cdr existing) (memory-type-property-flags heap-type)))
	       (push (cons (memory-type-heap-index heap-type)
			   (memory-type-property-flags heap-type))
		     found-indices))))
     (%get-memory-types (h (physical-device device))))
    (loop for type in found-indices
	  when (logtest VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT (cdr type))
	    do (if (device-local-allocators device)
		   (push (make-instance 'memory-allocator
					:device device
					:properties VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
			 (device-local-allocators device))
		   (setf (device-local-allocators device)
			 (list nil (make-instance 'memory-allocator
						  :device device
						  :properties VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT))))
	  unless (logtest VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT (cdr type))
	    do (if (non-device-local-allocators device)
		   (push (make-instance 'memory-allocator
					:device device
					:properties (cdr type))
			 (non-device-local-allocators device))
		   (setf (non-device-local-allocators device)
			 (list nil (make-instance 'memory-allocator
						  :device device
						  :properties (cdr type))))))))
	    
(defun %acquire-memory-miniscule (allocator size)
  (or (search-for-block allocator (memory-allocator-miniscule-free allocator) size)
      (%acquire-memory-tiny allocator size)))

(defun %acquire-memory-tiny (allocator size)
  (or (search-for-block allocator (memory-allocator-tiny-free allocator) size)
      (%acquire-memory-small allocator size)))

(defun %acquire-memory-small (allocator size)
  (or (search-for-block allocator (memory-allocator-small-free allocator) size)
      (%acquire-memory-ordinary allocator size)))

(defun %acquire-memory-ordinary (allocator size)
  (or (search-for-block allocator (memory-allocator-ordinary-free allocator) size)
      (%acquire-memory-medium allocator size)))

(defun %acquire-memory-medium (allocator size)
  (or (search-for-block allocator (memory-allocator-medium-free allocator) size)
      (%acquire-memory-large allocator size)))

(defun %acquire-memory-large (allocator size)
  (or (search-for-block allocator (memory-allocator-large-free allocator) size)
      (%acquire-memory-very-large allocator size)))

(defun %acquire-memory-very-large (allocator size)
  (or (search-for-block allocator (memory-allocator-very-large-free allocator) size)
      (%acquire-memory-huge allocator size)))

(defun %acquire-memory-huge (allocator size)
  (or (search-for-block allocator (memory-allocator-huge-free allocator) size)
      (%acquire-memory-gigantic allocator size)))

(defun %acquire-memory-gigantic (allocator size)
  (or (search-for-block allocator (memory-allocator-gigantic-free allocator) size)
      (%acquire-memory-ginormous allocator size)))

(defun %acquire-memory-ginormous (allocator size)
  (or (search-for-block allocator (memory-allocator-ginormous-free allocator) size)
      (%acquire-memory-outlandish allocator size)))

(defun %acquire-memory-outlandish (allocator size)
  (or (search-for-block allocator (memory-allocator-outlandish-free allocator) size)
      (%acquire-memory-obscene allocator size)))

(defun %acquire-memory-obscene (allocator size)
  (search-for-block allocator (memory-allocator-obscene-free allocator) size))

(defun destroy-memory-allocators (device)
  (loop for alctr in (append (rest (non-device-local-allocators device))
			     (rest (device-local-allocators device)))
	do
	   (%vk:vkdestroybuffer (h (device (memory-allocator-big-buffer alctr)))
				(h (memory-allocator-big-buffer alctr))
				(h (allocator (memory-allocator-big-buffer alctr))))
	   (%vk:vkfreememory (h (device (allocated-memory (memory-allocator-big-buffer alctr))))
			     (h (allocated-memory (memory-allocator-big-buffer alctr)))
			     (h (allocator (allocated-memory (memory-allocator-big-buffer alctr)))))
	finally (return (values))))
