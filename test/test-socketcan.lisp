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

;; bind socket to a specific CAN interface
(socket-bind *can-socket* (make-can-interface :name "vcan0"))
;; bind socket to any CAN interface
;;(socket-bind *can-socket* (make-can-interface :name "any"))

;; send frame over CAN socket
;; execute $ candump vcan0 
(socket-send *can-socket* (make-can-packet :id 101 :data #(1 2 3)))
;; check stdout

;; read frame from CAN socket 
(defparameter *can-frame* (make-can-packet))
(socket-recv *can-socket* *can-frame*) ;; should block
;; execute cansend vcan0 101#01.02.03.04.05.06.07.08
(pprint *can-frame*)
