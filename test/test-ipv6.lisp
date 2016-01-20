
(defpackage #:fsocket.test.ipv6
  (:use #:cl #:fsocket))

(in-package #:fsocket.test.ipv6)


;; --------------- UDP -----------------

(defun recv-ipv6 ()
  (let ((fd (open-socket :family :inet6))
        (pc (open-poll))
        (buff (make-array 512 :element-type '(unsigned-byte 8))))
    (poll-register pc (make-instance 'pollfd
                                     :fd fd
                                     :events (poll-events :pollin)))
    (unwind-protect
         (progn
           (socket-bind fd (sockaddr-in6))
           (format t "Bound to ~A~%" (socket-name fd))
           (when (poll pc :timeout 30000)
             (multiple-value-bind (cnt raddr) (fsocket:socket-recvfrom fd buff)
               (format t "recvfrom ~A ~A~%" cnt raddr)
               (packet:hd buff)
               (socket-sendto fd buff raddr))))
      (close-poll pc)            
      (close-socket fd)))
  (format t "DONE~%"))


(defun send-ipv6 (addr)
  (let ((fd (open-socket :family :inet6))
        (pc (open-poll))
        (buff (make-array 512 :element-type '(unsigned-byte 8))))
    (poll-register pc (make-instance 'pollfd
                                     :fd fd
                                     :events (poll-events :pollin)))
    (unwind-protect
         (progn
           (socket-bind fd (sockaddr-in6))
           (format t "Bound to ~A~%" (socket-name fd))
           (fsocket:socket-sendto fd buff addr)
           (when (poll pc :timeout 30000)
             (multiple-value-bind (cnt raddr) (fsocket:socket-recvfrom fd buff)
               (format t "recvfrom ~A ~A~%" cnt raddr)
               (packet:hd buff))))
      (close-poll pc)            
      (close-socket fd))))
  
  
;; ----------------- TCP -------------------


(defun recv-ipv6-tcp ()
  (let ((fd (open-socket :family :inet6 :type :stream))
        (pc (open-poll))
        (buff (make-array 512 :element-type '(unsigned-byte 8)))
        (cfd nil))
    (unwind-protect
         (progn
           (socket-bind fd (sockaddr-in6))
           (format t "Bound to ~A~%" (socket-name fd))
           (socket-listen fd)
           (poll-register pc (make-instance 'pollfd
                                            :fd fd
                                            :events (poll-events :pollin)))
           (when (poll pc :timeout 30000)
             ;; ready to accept
             (multiple-value-bind (c raddr) (socket-accept fd)
               (setf cfd c)
               (let ((cnt (fsocket:socket-recv cfd buff)))
                 (format t "recvfrom ~A ~A~%" cnt raddr)
                 (packet:hd buff)
                 (socket-send cfd buff)))))
      (close-poll pc)
      (when cfd (socket-shutdown cfd) (close-socket cfd))
      (close-socket fd)))

  (format t "DONE~%"))


(defun send-ipv6-tcp (port)
  (let ((fd (open-socket :family :inet6 :type :stream))
        (pc (open-poll))
        (buff (make-array 512 :element-type '(unsigned-byte 8))))
    (unwind-protect
         (progn
           (socket-bind fd (sockaddr-in6))
           (format t "Bound to ~A~%" (socket-name fd))
           (socket-connect fd (sockaddr-in6 #(0 0 0 0 0 0 0 1) port))
           (poll-register pc (make-instance 'pollfd
                                            :fd fd
                                            :events (poll-events :pollin)))
           (fsocket:socket-send fd buff)
           (when (poll pc :timeout 30000)
             (let ((cnt (fsocket:socket-recv fd buff)))
               (format t "recvfrom ~A~%" cnt)
               (packet:hd buff))))
      (close-poll pc)            
      (close-socket fd))))
