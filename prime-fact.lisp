(load "prime-number.lisp")

(defparameter *prime-list* (sieve-of-eratosthenes))

(defun prime-factorization (n &optional (pos 0) (result (list nil)) (is-failed nil))
  (let* ((prime (nth pos *prime-list*))
         (tmp (floor (/ n (float prime))))
         (remember (mod n prime)))
    (format t "prime: ~A, quotient: ~A, remember: ~A~%" prime tmp remember)
    (cond ((/= remember 0)
           (if is-failed
               (cdr (reverse result))
               (prime-factorization n (incf pos) result t)))
          ((= remember 0)
           (push prime result)
           (prime-factorization tmp pos result)))))

(defun positive-divisor (l)
  (let ((tmp-list (list nil)))
    (dolist (x l)
      (push (1+ x) tmp-list))
    (cdr (reverse tmp-list))))

(defun main ()
  (let ((tmp (prime-factorization 6)))
    (print tmp)
    (print (positive-divisor tmp))))
