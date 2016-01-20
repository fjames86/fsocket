
(defpackage #:fsocket.test.tcp
  (:use #:cl #:fsocket))

(in-package #:fsocket.test.tcp)

;; this shows how to do a non-blocking connect and accept

(defun nbconnect (addr)
  (let ((fd (open-socket :type :stream))
        (pc (open-poll)))
    (socket-bind fd (sockaddr-in))
    (poll-register pc (make-instance 'pollfd
                                     :fd fd
                                     :events (poll-events :pollin :pollout)))
    ;; the fd is now in non-blocking mode 
    (unwind-protect
         (cond
           ((socket-connect fd addr)
            (format t "Connected immediately~%")
            (socket-shutdown fd))
           (t
            (format t "Connecting...~%")
            (let ((pfds (poll pc :timeout 1000)))
              (if pfds
                  (dolist (event (poll-events (pollfd-revents (first pfds))))
                    (case event
                      (:pollout (let ((sts (socket-option fd :socket :error)))
                                  (if (zerop sts)
                                      (format t "Connected~%")
                                      (format t "Error connecting ~A~%" sts))
                                  (socket-shutdown fd)))))
                  (format t "Timeout connecting.~%")))))
      (close-poll pc)
      (close-socket fd))))


(defun nbaccept ()
  (let ((fd (open-socket :type :stream))
        (pc (open-poll)))
    (socket-bind fd (sockaddr-in nil 8000))
    (socket-listen fd)
    (poll-register pc (make-instance 'pollfd
                                     :fd fd
                                     :events (poll-events :pollin :pollout)))
    (unwind-protect
         (progn
           (multiple-value-bind (cfd raddr) (socket-accept fd)
             (cond
               (cfd
                (format t "Accepted connection from ~A~%" raddr)
                (socket-shutdown cfd)
                (close-socket cfd))
               (t
                (format t "Accepting...~%")
                (let ((pfds (poll pc :timeout 20000)))
                  (if pfds
                      (dolist (event (poll-events (pollfd-revents (first pfds))))
                        (case event
                          (:pollin
                           (format t "Accepted~%")
                           (multiple-value-bind (cfd2 raddr2) (socket-accept fd)
                             (format t "RADDR: ~A~%" raddr2)
                             (socket-shutdown cfd2)
                             (close-socket cfd2)))))
                      (format t "Timeout~%")))))))                      
      (close-socket fd)
      (close-poll pc))))
