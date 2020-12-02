(in-package :vktk)

(defcstruct xform-uniform
  (matrix (:struct mat4))
  (nverts :uint32))
	
(defun compute-transformation (device matrix vertex-array)
  (assert (zerop (mod (length vertex-array) 3)))
  (let ((output-array)
	(nverts (/ (length vertex-array) 3))
	(dsl)
	(pipeline-layout)
	(shader-module)
	(pipeline)
	(command-pool)
	(command-buffer)
	(ds)
	(uniform-buffer)
	(storage-buffer)
	(storage-buffer-memory))

    (unwind-protect
	 (multiple-value-bind (queue family-index)
	     (compute-queue device)
	   
	   (setq dsl (create-descriptor-set-layout
		      device
		      :bindings
		      (list (make-instance 'sample-uniform-buffer-for-compute-shader-dsl-binding)
			    (make-instance 'sample-input-storage-buffer-for-compute-shader-dsl-binding)))
		 pipeline-layout (create-pipeline-layout device (list dsl))
		 shader-module (create-shader-module-from-file device (concatenate 'string *assets-dir* "shaders/comp.spv"))
		 pipeline (create-compute-pipeline device pipeline-layout shader-module)
		 command-pool (second (assoc family-index (command-pools device)))
		 command-buffer (elt (command-buffers command-pool) 0))

	   (time
	    (unwind-protect
		 (let* ((size (* (load-time-value (foreign-type-size :float)) (length vertex-array))))
	   
		   (setq storage-buffer (create-buffer-1 device size VK_BUFFER_USAGE_STORAGE_BUFFER_BIT)
			 storage-buffer-memory (allocate-buffer-memory
						device storage-buffer
						(logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
							VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)))
	    
		   (bind-buffer-memory device storage-buffer storage-buffer-memory)
		   (setf (allocated-memory storage-buffer) storage-buffer-memory)

		   (with-foreign-object (pp-data :pointer)
		     (vkMapMemory (h device) (h storage-buffer-memory) 0 size 0 pp-data)
		     (sb-sys:with-pinned-objects
			 (vertex-array)
		       (memcpy (mem-aref pp-data :pointer) (sb-sys:vector-sap vertex-array) size))
		     (vkUnmapMemory (h device) (h storage-buffer-memory)))
				
		   (let* ((uniform-range (* (load-time-value (foreign-type-size :float)) 16)))
	     
		     (setq uniform-buffer (create-uniform-buffer device uniform-range))

		     (with-foreign-object (p-uniform '(:struct xform-uniform))
		       (let ((p-matrix (foreign-slot-pointer p-uniform '(:struct xform-uniform) 'matrix)))
			 
			 (loop for i from 0 below 4
			    do (loop for j from 0 below 4
				  do (setf (mem-aref p-matrix :float (+ (* j 4) i)) (mcref4 matrix i j))
				  finally (setf (foreign-slot-value p-uniform '(:struct xform-uniform) 'nverts) nverts))))

		       (copy-uniform-buffer-memory device p-uniform
						   (allocated-memory uniform-buffer) uniform-range))
	    
		     (setq ds (create-descriptor-set device (list dsl)
						     (descriptor-pool device)
						     :descriptor-buffer-info
						     (list (make-instance 'descriptor-uniform-buffer-info
									  :buffer uniform-buffer
									  :range uniform-range)
							   (make-instance 'descriptor-storage-buffer-info
									  :buffer storage-buffer
									  :range size))))
		

		     (begin-command-buffer command-buffer)
	   
		     (vkCmdBindPipeline (h command-buffer) VK_PIPELINE_BIND_POINT_COMPUTE (h pipeline))
	   
		     (cmd-bind-descriptor-sets command-buffer pipeline-layout (list ds) :compute)
	     
		     (vkCmdDispatch (h command-buffer) nverts 1 1)
	     
		     (vkEndCommandBuffer (h command-buffer))
	     
		     (with-vk-struct (p-submit-info VkSubmitInfo)
		       (with-foreign-slots ((vk::commandBufferCount
					     vk::pCommandBuffers)
					    p-submit-info
					    (:struct VkSubmitInfo))
		 
			 (with-foreign-object (p-command-buffer 'VkCommandBuffer)
			   (setf (mem-aref p-command-buffer 'VkCommandBuffer) (h command-buffer))
		   
			   (setf vk::commandBufferCount 1
				 vk::pCommandBuffers p-command-buffer)
		   
			   (vkQueueSubmit (h queue) 1 p-submit-info VK_NULL_HANDLE)
		       
			   (setq output-array
				 (make-array (length vertex-array) :element-type 'single-float))
		       
			   (vkQueueWaitIdle (h queue))

			   (with-foreign-object (pp-data :pointer)
			     (vkMapMemory (h device) (h (allocated-memory storage-buffer)) 0 size 0 pp-data)
			     (sb-sys:with-pinned-objects
				 (output-array)
			       (memcpy (sb-sys:vector-sap output-array) (mem-aref pp-data :pointer) size))
			     (vkUnmapMemory (h device) (h (allocated-memory storage-buffer)))))))))
	      
	      #+NIL(when ds (free-descriptor-sets (list ds) (descriptor-pool device))) ;; just return to pool
	      (when storage-buffer
		(destroy-buffer storage-buffer))
	      (when uniform-buffer
		(destroy-buffer uniform-buffer)))))
      
      (when pipeline (destroy-pipeline pipeline))
      (when shader-module (destroy-shader-module shader-module))
      (when pipeline-layout (destroy-pipeline-layout pipeline-layout))
      (when dsl (destroy-descriptor-set-layout dsl)))
    output-array))

#+EXAMPLE
(vktk::compute-transformation *d* (3d-matrices:nmrotate (3d-matrices::meye 4) (3d-vectors::vec3 0 0 1) 42) vktk::*test-verts*)
