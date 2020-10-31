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
(asdf:make "cffi")
(asdf:make "fsocket")

;; define package
(defpackage #:fsocket.test.can
  (:use #:cl #:fsocket))

(in-package #:fsocket.test.can)

;; create CAN socket
(defparameter *can-socket* (open-socket :family :can :type :raw))

(defun test-specific-can-interface (name)
  (socket-bind *can-socket* (make-can-interface :name name))
  (socket-send *can-socket* (make-can-packet :id 101 :data #(1 2 3)))
  (let ((frame (make-can-packet)))
    (socket-recv *can-socket* frame) ;; should block
    (pprint frame)))

(defun test-general-can-interface ()
  (socket-bind *can-socket* (make-can-interface :name "any"))
  (socket-sendto *can-socket* (make-can-packet :id 101 :data #(1 2 3)) (make-can-interface :name "vcan0"))
  (let ((frame (make-can-packet)))
    (socket-recvfrom *can-socket* frame) ;; should block
    (pprint frame)))

(test-specific-can-interface "vcan0")
(test-general-can-interface)
