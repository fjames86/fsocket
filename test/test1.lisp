;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file just shows basic usage by implementing an echo server for UDP and TCP.

(defpackage #:fsocket.test1
  (:use #:cl #:fsocket))

(in-package #:fsocket.test1)

(defun udp-echo (port timeout)
  "Implements a simple UDP echoing server. Serves one request then exits."
  (let ((pc (open-poll)))
    (unwind-protect
         (progn
           (let ((sock (open-socket :type :datagram)))
             (socket-bind sock (make-sockaddr-in :port port))
             (poll-register pc
                            (make-pollfd sock
                                         :events (poll-events :pollin))))
           (do ((i 0 (1+ i)))
               ((= i 5))
             (let ((fds (poll pc :timeout timeout)))
               (dolist (pollfd fds)
                 (dolist (event (poll-events (pollfd-revents pollfd)))
                   (case event
                     (:pollin
                      ;; data to read
                      (let ((buffer (make-array 1024)))
                        (multiple-value-bind (count addr) (socket-recvfrom (pollfd-fd pollfd) buffer)
                          (format t "Received ~A bytes from ~A~%" count addr)
                          ;; echo back
                          (let ((count (socket-sendto (pollfd-fd pollfd)
                                                      buffer
                                                      addr
                                                      :start 0 :end count)))
                            (format t "Replied with ~A bytes back to ~A~%" count addr)))))))))))
      (dolist (pollfd (poll-context-fds pc))
        (close-socket (pollfd-fd pollfd)))
      (close-poll pc))))


;; ----------------------------------------

(defun universal-time-string ()
  (multiple-value-bind (sec min hour date month year) (decode-universal-time (get-universal-time))
    (format nil "~A-~A-~A ~A:~A:~A"
            year month date hour min sec)))

(defclass test-stream-pollfd (pollfd)
  ((buffer :initarg :buffer :initform nil :accessor test-buffer)
   (count :initarg :count :initform 0 :accessor test-count)))

(defclass test-listening-pollfd (pollfd)
  ())

(defun tcp-echo (port)
  (let ((pc (open-poll)))
    (unwind-protect
         (progn
           ;; allocate a TCP socket and start listening
           (let ((sock (open-socket :type :stream)))
             (socket-bind sock (make-sockaddr-in :port port))
             (socket-listen sock)
             (poll-register pc (make-instance 'test-listening-pollfd
                                              :fd sock
                                              :events (poll-events :pollin))))
           
           ;; poll and process the events
           (do ((done nil)
                (i 0 (1+ i)))
               ((or done (> i 10)))
             (doevents (pollfd event) (poll pc :timeout 1000)
               (case event
                 (:pollin
                  ;; data to read
                  (etypecase pollfd
                    (test-listening-pollfd
                     ;; pollin on an listening socket means ready to accept a connection
                     (multiple-value-bind (conn addr) (socket-accept (pollfd-fd pollfd))
                       (format t "Accepted connection from ~A~%" addr)
                       (poll-register pc (make-instance 'test-stream-pollfd
                                                        :fd conn
                                                        :events (poll-events :pollin :pollout)))))
                    (test-stream-pollfd
                     ;; read from the connection
                     (let ((buffer (make-array 512)))
                       (let ((count (socket-recv (pollfd-fd pollfd) buffer :start (test-count pollfd))))
                         (format t "~A Received ~A bytes~%" (universal-time-string) count)
                         (cond
                           ((zerop count)
                            ;; recving 0 bytes indicates a graceful close of the other side
                            (close-socket (pollfd-fd pollfd))
                            (poll-unregister pc pollfd))
                           (t
                            ;; put the buffer away and schedule a send
                            (unless (test-buffer pollfd)
                              (setf (test-buffer pollfd) buffer))
                            (incf (test-count pollfd) count))))))))
                 (:pollout
                  ;; ready to write, reply with the count bytes (if any)
                  (typecase pollfd
                    (test-stream-pollfd 
                     (when (and (test-count pollfd) (not (zerop (test-count pollfd))))
                       (format t "Sending ~A bytes back~%" (test-count pollfd))
                       (socket-send (pollfd-fd pollfd)
                                    (test-buffer pollfd)
                                    :end (test-count pollfd))
                       (format t "setting done flag~%")
                       (setf done t)))))))))
      ;; close each socket 
      (dolist (pollfd (poll-context-fds pc))
        (close-socket (pollfd-fd pollfd)))
      ;; close the poll context 
      (close-poll pc))))
