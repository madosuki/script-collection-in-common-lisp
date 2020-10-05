(defun fact (n)
  (reduce #'* (loop for i upfrom 1 to n collect i)))
