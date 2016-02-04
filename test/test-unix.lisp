
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

(defun client-nb (path)
  (let ((fd (open-socket :family :unix 
                         :type :stream))
        (pc (open-poll)))
    (poll-register pc (make-instance 'pollfd 
                                     :fd fd 
                                     :events (poll-events :pollin :pollout)))
    (unwind-protect 
         (let ((buffer (make-array 128 :element-type '(unsigned-byte 8))))
           (format t "Connecting...~%")
           (when (socket-connect fd path)
             (format t "Connected immediately~%")
             (socket-send fd (babel:string-to-octets "Hello from Lisp")))
           (doevents (pfd event) (poll pc :timeout 1000)
             (format t "EVENT: ~A~%" event)
             (case event 
               (:pollin
                (let ((cnt (socket-recv fd buffer)))
                  (format t "RECV ~A~%" (babel:octets-to-string buffer :end cnt))))
               (:pollout 
                (format t "Connected~%")
                (socket-send fd (babel:string-to-octets "Hello from Lisp"))))))
      (close-socket fd)
      (close-poll pc))))


(defclass un-pollfd (pollfd)
  ((addr :initarg :addr :reader un-pollfd-addr)))

(defun server-nb (path)

  (let ((fd (open-socket :family :unix 
                         :type :stream))
        (pc (open-poll)))
    (poll-register pc (make-instance 'pollfd 
                                     :fd fd 
                                     :events (poll-events :pollin)))
    (unwind-protect 
         (let ((buffer (make-array 128 :element-type '(unsigned-byte 8))))
           (socket-bind fd path)
           (socket-listen fd)
           (format t "Accepting..~%")
           (multiple-value-bind (conn raddr) (socket-accept fd)
             (when conn                 
               (format t "Accepted immediately from ~A~%" raddr)
               (poll-register pc 
                              (make-instance 'un-pollfd 
                                              :fd conn 
                                              :events (poll-events :pollin)
                                              :addr raddr))))
           (do ((done nil))
               (done)
             (doevents (pfd event) (poll pc :timeout 1000)
               (format t "EVENT ~A~%" event)
               (typecase pfd 
                 (un-pollfd 
                  ;; connected socket 
                  (let ((cnt (socket-recv (pollfd-fd pfd) buffer)))
                    (format t "RECV ~A~%" cnt)
                    (socket-send (pollfd-fd pfd) buffer :end cnt)
                    (format t "Closing connection~%")
                    (socket-shutdown (pollfd-fd pfd))
                    (close-socket (pollfd-fd pfd))
                    (poll-unregister pc pfd)
                    (setf done t)))
                 (pollfd 
                  ;; listening socket ready to accept 
                  (format t "Ready to accept~%")
                  (multiple-value-bind (conn raddr) (socket-accept (pollfd-fd pfd))
                    (when conn
                      (format t "Accpted from ~A~%" raddr)
                      (poll-register pc 
                                     (make-instance 'un-pollfd 
                                                    :fd conn
                                                    :events (poll-events :pollin)
                                                    :addr raddr)))))))))
      (close-socket fd)
      (close-poll pc))))
