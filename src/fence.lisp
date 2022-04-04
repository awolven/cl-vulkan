(in-package :vk)

(defmacro with-fences ((swapchain) &body body)
  (let ((fence-sym (gensym))
        (swapchain-sym (gensym))
        (result-sym (gensym))
        (break-sym (gensym))
        (continue-sym (gensym)))
    `(let* ((,swapchain-sym ,swapchain)
            (,fence-sym (fence (elt (frame-resources ,swapchain-sym) (current-frame ,swapchain-sym)))))

       (tagbody
          ,continue-sym
          (let ((,result-sym
                  (with-foreign-object (p-fences 'VkFence)
                    (setf (mem-aref p-fences 'VkFence) (h ,fence-sym))
                    (vkWaitForFences (h (device ,swapchain-sym)) 1 p-fences VK_TRUE 100))))

            ;; probably can set wait time to uint32 max and eliminate this tagbody
            (when (eq ,result-sym VK_SUCCESS)
              (go ,break-sym))
            (when (eq ,result-sym VK_TIMEOUT)
              (go ,continue-sym))
            (check-vk-result ,result-sym))

          ,break-sym
          ,@body))))
