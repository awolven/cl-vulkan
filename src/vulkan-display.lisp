(in-package :vk)

(defmethod initialize-instance :after ((instance vulkan-enabled-display-mixin) &rest initargs)
  (declare (ignore initargs))

  (let* ((helper-window (clui::helper-window instance))
	 (surface (render-surface helper-window))
	 (device (default-logical-device instance)))
    
    (unless (paired-gpu surface)
      ;; helper window surface has not been initialized yet
      ;; because we didn't have logical device when it was created.
      ;; so initialize it so that we can get the surface-format to
      ;; create the render pass properly
      (let* ((gpu (physical-device device))
	     (index (get-queue-family-index-with-wsi-support gpu surface)))
	(initialize-window-surface surface gpu index)))

    (let ((depth-format (find-supported-depth-format (physical-device device))))
      (unless (display-default-render-pass instance)
	(setf (display-default-render-pass instance)
	      (let ((format-enum (surface-format-format (find-supported-format surface))))
		(create-render-pass device format-enum
				    :color-attachments (list (make-instance 'color-attachment
									    :name :the-color-attachment
									    :samples (max-usable-sample-count device)
									    :format format-enum
									    :final-layout VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL))
				    :depth-attachments (list (make-instance 'depth-attachment
									    :name :3d-depth-attachment
									    :samples (max-usable-sample-count device)
									    :format depth-format)
							     (make-instance 'depth-attachment
									    :name :2d-depth-attachment
									    :samples (max-usable-sample-count device)
									    :format depth-format))
				    :subpasses (list (make-instance 'subpass
								    :name :3d-subpass
								    :color-attachments (list :the-color-attachment)
								    :depth-attachments (list :3d-depth-attachment))
						     (make-instance 'subpass
								    :name :2d-subpass
								    :color-attachments (list :the-color-attachment)
								    :depth-attachments (list :2d-depth-attachment)
								    :dependencies (list :subpass-dependency)))
				    :subpass-dependencies
				    (list (make-instance 'vk::subpass-dependency
							 :src-subpass 0
							 :dst-subpass 1
							 :src-stage-mask VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
							 :dst-stage-mask VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
							 :src-access-mask VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
							 :dst-access-mask VK_ACCESS_SHADER_READ_BIT)))))))
    (values)))
