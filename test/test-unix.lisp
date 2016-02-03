
;;; Example of UNIX sockets. Only works on posix systems (linux, freebsd, darwin).

(defpackage #:fsocket.test.unix 
  (:use #:cl #:fsocket))

(in-package #:fsocket.test.unix)

(defun client (path)
  (let ((fd (open-socket :family :unix 
                         :type :stream)))
    (unwind-protect 
         (progn
           (format t "Connecting...~%")
           (socket-connect fd path)
           (format t "Connected~%")
           (socket-send fd 
                        (babel:string-to-octets "Hello from Lisp"))
           (let ((buffer (make-array 128 :element-type '(unsigned-byte 8))))
             (let ((cnt (socket-recv fd buffer)))
               (format t "RECV: ~A~%" (babel:octets-to-string buffer :end cnt)))))
      (close-socket fd))))
           
;; TODO: write the server 

