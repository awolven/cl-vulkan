(in-package :vk)

(declaim (inline clampf))
(defun clampf (number)
  "Clamp real number to single-float limits."
  (block nil
    (when (typep number 'single-float)
      (return number))
    (etypecase number
      (real
       (etypecase number
	 (double-float
	  (when (= number 0.0d0)
	    (return 0.0f0))
	  (when (< (cl:the double-float (load-time-value (/ least-negative-single-float 2.0d0)))
		   number
		   (cl:the double-float (load-time-value (/ least-positive-single-float 2.0d0))))
	    (return 0.0f0))
	  (when (< number 0.0d0)
	    (when (> number (cl:the single-float least-negative-single-float))
	      (return least-negative-single-float))
	    (when (< number (cl:the single-float most-negative-single-float))
	      (return most-negative-single-float))
	    (return (coerce number 'single-float)))
	  (when (< number (cl:the single-float least-positive-single-float))
	    (return least-positive-single-float))
	  (when (> number (cl:the single-float most-positive-single-float))
	    (return most-positive-single-float))
	  (coerce number 'single-float))      
	 (integer
	  (when (= number 0)
	    (return 0.0f0))
	  (when (< number (cl:the single-float most-negative-single-float))
	    (return most-negative-single-float))
	  (when (> number (cl:the single-float most-positive-single-float))
	    (return most-positive-single-float))
	  (coerce number 'single-float))
	 (rational
	  (when (< (cl:the double-float (load-time-value (/ least-negative-single-float 2.0d0)))
		   number
		   (cl:the double-float (load-time-value (/ least-positive-single-float 2.0d0))))
	    (return 0.0f0))
	  (when (< number 0)
	    (when (> number (cl:the single-float least-negative-single-float))
	      (return least-negative-single-float))
	    (return (coerce number 'single-float)))
	  (when (< number (cl:the single-float least-positive-single-float))
	    (return least-positive-single-float))
	  (coerce number 'single-float)))))))
