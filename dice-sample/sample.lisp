(ql:quickload :cl-ppcre)
(ql:quickload :ironclad)

(defun dice (dice-num max-num)
  (labels ((dice-function (d-n m-num &optional (result nil) (count 0))
             (if (= count d-n)
                 result
                 (let ((dice-result (1+ (ironclad:strong-random m-num))))
                   (if (listp result)
                       (dice-function d-n m-num (append result (list dice-result)) (1+ count))
                       (dice-function d-n m-num (list dice-result) (1+ count)))))))
    (dice-function dice-num max-num)))

(defun generate-dice-string (d-n m-num dice-result-list)
  (let ((dice-result-string ""))
    (if (> d-n 1)
        (progn
          (dolist (x dice-result-list)
            (setq dice-result-string
                  (concatenate 'string dice-result-string
                               (if (string= dice-result-string "") "" "+")
                               (write-to-string x))))
          (setq dice-result-string
                (concatenate 'string
                             (write-to-string (reduce #'+ dice-result-list))
                             "(" dice-result-string ")")))
        (setq dice-result-string (write-to-string (car dice-result-list))))
    (concatenate 'string "[" (write-to-string d-n) "D" (write-to-string m-num) ":" dice-result-string "]")))

(defun search-num-on-dice-strings (s)
  (let ((left-num-list (list nil))
        (right-num-list (list nil))
        (s-list (coerce s 'list))
        (is-end-of-left nil))
    (dolist (x s-list)
      (if (or (equal #\D x) (equal #\d x))
          (setq is-end-of-left t)
          (if is-end-of-left
              (push x right-num-list)
              (push x left-num-list))))
    (setq left-num-list (cdr (reverse left-num-list)))
    (setq right-num-list (cdr (reverse right-num-list)))
    (values
     (parse-integer (coerce left-num-list 'string) :junk-allowed t)
     (parse-integer (coerce right-num-list 'string) :junk-allowed t))))

(defun apply-dice (text &optional (is-in-name nil))
  (multiple-value-bind (start-position end-position group)
      (ppcre:scan "!(\\d{1,2})[dD](\\d{1,3})" text)
    (unless start-position
      (return-from apply-dice text))
    (let* ((tmp-string (subseq text (aref group 0) end-position)))
      (multiple-value-bind (left right)
          (search-num-on-dice-strings tmp-string)
        (unless (and (null left) (null right))
          (if (and (< left 11) (< right 101))
              (let ((tmp (generate-dice-string left right (dice left right)))
                    (begin-tag (if is-in-name "" "<b>"))
                    (end-tag (if is-in-name "" "</b>")))
                (apply-dice
                 (concatenate 'string (subseq text 0 start-position) begin-tag tmp end-tag (subseq text end-position))
                 is-in-name))
              (apply-dice
               (concatenate 'string (subseq text 0 start-position)
                            "[is over]"
                            (subseq text end-position))
               is-in-name)))))))

(defun main ()
  (let ((sample-text "!20d100 !2D1000 desu"))
    (print (apply-dice sample-text))))
