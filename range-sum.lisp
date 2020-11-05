(defun sum-between-range (min-number max-number)
  (- (/ (* max-number (+ max-number 1)) 2)
     (/ (* min-number (- min-number 1)) 2)))

(defun test-sum-between-range ()
  (if (= (sum-between-range 1 3) 6)
      t
      nil))
