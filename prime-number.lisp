;; return value is list
(defun sieve-of-eratosthenes (&optional (target-number 128))
  (let ((search-list (loop for i from 2 to target-number
                           if (/= (mod i 2) 0)
                             collect i))
        (result (list 2))
        (range-max (sqrt target-number)))
    (labels ((calculator ()
               (if (or (null (car search-list)) (>= (sqrt (car search-list)) range-max))
                   (nreverse result)
                   (progn
                     (push (car search-list) result)
                     (setq search-list (remove-if #'(lambda (n) (= (mod n (car result)) 0)) search-list))
                     (calculator)))))
      (calculator))))
