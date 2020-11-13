;;;; This file shows how to handle CAN traffic using SocketCAN

;; To set up a virtual CAN interface (ex. "vcan0"):
;;    $ modprobe vcan
;;    $ sudo ip link add dev vcan0 type vcan
;;    $ sudo ip link set up vcan0

;; To set up a native CAN interface (ex. "can0"):
;;    $ sudo ip link set can0 type can bitrate 125000
;;    $ sudo ip link set up can0

;; For testing use SocketCAN utilities from https://github.com/linux-can/can-utils
  ;; Send CAN frame:
  ;; $ cansend vcan0 101#01.02.03.04.05.06.07.08
  ;; Read CAN frame (will block):
  ;; $ candump vcan0

;; load libraries
(eval-when (:compile-toplevel)
  (asdf:make "cffi")
  (asdf:make "fsocket"))

;; for testing purpose
(eval-when (:compile-toplevel)
  (ql:quickload :bt-semaphore))

;; define package
(defpackage #:fsocket.test.can
  (:use #:cl #:fsocket))

(in-package #:fsocket.test.can)
  
;;;; CAN loopback tests
(defun loopback-on-specific-can-interface (name)
  ;; wait for a frame
  (bt:make-thread
   (lambda ()
     (with-can-socket (sckt name)
       (let ((frame (make-can-packet)))	 	     
	 (socket-recv sckt frame)
	 (assert (and
		  (= 101 (fsocket::can-packet-id frame))
		  (equalp #(1 2 3) (fsocket::can-packet-data frame))
		  (eql NIL (fsocket::can-packet-origin frame))))))))
  (sleep 0.001)
  ;; send a frame
  (bt:make-thread
   (lambda ()     
     (with-can-socket (sckt name)
       (socket-send sckt (make-can-packet :id 101 :data #(1 2 3)))))))

(defun loopback-on-general-can-interface ()
  ;; wait for a frame
  (bt:make-thread
   (lambda ()
     (with-can-socket (sckt)
       (let ((frame (make-can-packet)))	 	     
	 (socket-recvfrom sckt frame) ;; use recvfrom to get origin of data
	 (assert (and
		  (= 101 (fsocket::can-packet-id frame))
		  (equalp #(1 2 3) (fsocket::can-packet-data frame))
		  (string= "vcan0" (fsocket::can-packet-origin frame))))))))
  (sleep 0.001)
  ;; send a frame
  (bt:make-thread
   (lambda ()     
     (with-can-socket (sckt)
       ;; use sendto to specify the target interface
       (socket-sendto sckt (make-can-packet :id 101 :data #(1 2 3)) (make-can-interface :name "vcan0"))))))

;; run tests
(loopback-on-specific-can-interface "vcan0")
(loopback-on-general-can-interface) 
	
