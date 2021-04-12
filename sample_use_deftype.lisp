(deftype bool () `(member false true))

(defun my-some-p (v)
  (if (eq (car v) 'some)
      t
      nil))

(deftype option-some () `(and cons (satisfies my-some-p)))
(deftype option-none () `(member none))
(deftype option () `(or option-some option-none))

(defmacro contain-some (v)
  `(cons 'some ,v))

(defun fact (n &optional (result 1))
  (when (< n 0)
    (return-from fact 'none))
  (if (or (= n 0) (= n 1))
      (contain-some result)
      (fact (- n 1) (* result n))))

(defun main ()
  (let ((target (cons 'some 0)))
    (typecase target
      (option-some
       (print "some"))
      (option-none
       (print "none"))
      (t (print "other")))
    (typecase 'none
      (option (print "option"))
      (option-none (print "none"))
      (t (print "other")))
    (let ((fact-result (fact 5)))
      (typecase fact-result
        (option-some
         (print (cdr fact-result)))))))
