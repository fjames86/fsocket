;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:fsocket)

(defconstant +af-inet+ 2)

;; AF_INET6 seems to have different values on different platforms...
#+(or win32 windows)(defconstant +af-inet6+ 23)
#+linux(defconstant +af-inet6+ 10)
#+freebsd(defconstant +af-inet6+ 28)

(defconstant +sock-stream+ 1)
(defconstant +sock-dgram+ 2)

(defconstant +ipproto-ip+ 0)
(defconstant +ipproto-ip6+ 41)

(defconstant +ipproto-udp+ 17)
(defconstant +ipproto-tcp+ 6)
