(defun fact (x &optional (y 1))
  (if (<= x 1)
      y
      (fact (- x 1) (* x y))))

(defun combination (l r)
  (if (or (< l 1) (< r 1))
      nil
      (let ((numerator (fact l))
            (denominator (fact (- l r))))
        (/ numerator denominator))))

