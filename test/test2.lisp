;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


;;; This file shows how to do multicast UDP

(defpackage #:fsocket.test2
  (:use #:cl #:fsocket))

(in-package #:fsocket.test2)


(defun mcast-send-socket (ad)
  (let ((sock (open-socket)))
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (close-socket sock)
                            ;; decline to handler the error by letting control pass through
                            nil)))
      
      ;; disable on loopback
      (setf (socket-option sock :ip :ip-multicast-loop) nil)

      ;; select interface to send on 
      (format t "Selecting interface ~A ~S ~%" (sockaddr-in-addr (car (adapter-unicast ad))) sock)
      (setf (socket-option sock :ip :ip-multicast-if)
            (sockaddr-in-addr (car (adapter-unicast ad))))

      ;; set multicast ttl
      (setf (socket-option sock :ip :ip-multicast-ttl) 2)

      ;; bind
      (socket-bind sock (make-sockaddr-in))

      sock)))

(defun mcast-recv-socket (mcaddr)
  (let ((sock (open-socket)))
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (close-socket sock)
                            ;; decline to handler the error by letting control pass through
                            nil)))

      ;; reuse address
      (setf (socket-option sock :socket :reuseaddr) t)
      
      ;; bind to local address
      (socket-bind sock (make-sockaddr-in :port (sockaddr-in-port mcaddr)))
            
      ;; join multicast address on all ethernet interfaces
      (let ((ads (list-adapters)))
        (dolist (ad ads)
          (when (eq (adapter-type ad) :ethernet)
            ;; tell the interface to join the group                  
            (setf (socket-option sock :ip :mcast-join-group)
                  ;; argument is (ifindex mcaddr)
                  (list (adapter-index ad)
                        mcaddr)))))

      sock)))


(defparameter *mcaddr* (make-sockaddr-in :addr #(239 255 23 23) :port 9006))

(defun mcast-test (mcaddr &key (period 5))
  (let ((pc (open-poll))
        (rsock (mcast-recv-socket mcaddr))
        (ssocks (mapcan (lambda (ad)
                          (when (eq (adapter-type ad) :ethernet)
                            (list (mcast-send-socket ad))))
                        (list-adapters))))
    ;; register the receiving socket 
    (poll-register pc (make-pollfd rsock :events (poll-events :pollin)))
    
    (unwind-protect
         (progn
           (do ((i 0 (1+ i))
                (buffer (make-array 32 :initial-element 12))
                (now (get-universal-time) (get-universal-time))
                (next-send (+ (get-universal-time) period)))
               ((= i 50))
             (format t "Polling~%")

             ;; periodically multicast a message on each interface
             (when (> now next-send)
               (dotimes (i (length buffer))
                 (setf (aref buffer i) 12))
               (dolist (s ssocks)
                 (format t "Sending on ~S~%" s)
                 (socket-sendto s buffer mcaddr))
               (incf next-send period))
             
             ;; poll for messages on the receiving socket
             (doevents (pollfd event) (poll pc :timeout 1000)
               (case event
                 (:pollin
                  ;; data to read
                  (multiple-value-bind (count addr) (socket-recvfrom rsock buffer)
                    (format t "Received ~A from ~A~%" count addr)))))))
      
      ;; close the sockets
      (close-socket rsock)
      (dolist (s ssocks)
        (close-socket s))
      ;; close the poll context 
      (close-poll pc))))
          
  
