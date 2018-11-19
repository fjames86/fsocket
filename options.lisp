;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file defines all the varios socket options

(in-package #:fsocket)

;; here we define the various socketoption methods. We assume %getsockopt and %setsockopt have already
;; been defined (either in windows.lisp or posix.lisp)

(defgeneric socket-option (sock level option)
  (:documentation "Get a socket option. 
SOCK ::= socket.
LEVEL ::= symbol naming a level.
OPTION ::= symbol naming a socket option.

Returns the value of the named option."))

(defgeneric (setf socket-option) (value sock level option)
  (:documentation "Set a socket option.
SOCK ::= socket
LEVEL ::= symbol naming a socket level.
OPTION ::= symbol naming a socket option.
VALUE ::= value to set."))

(defun get-socket-option-boolean (sock level option)
  (with-foreign-objects ((vbuf :uint8 4)
                         (vlen :uint32))
    (setf (mem-aref vlen :uint32) 4)
    (let ((sts (%getsockopt sock 
                            level
                            option
                            vbuf
                            vlen)))
      (if (= sts +socket-error+)
          (get-last-error)
          (mem-aref vbuf :boolean)))))

(defun get-socket-option-int32 (sock level option)
  (with-foreign-objects ((vbuf :int32)
                         (vlen :uint32))
    (setf (mem-aref vlen :uint32) 4)
    (let ((sts (%getsockopt sock 
                            level
                            option
                            vbuf
                            vlen)))
      (if (= sts +socket-error+)
          (get-last-error)
          (mem-aref vbuf :int32)))))

(defun get-socket-option-uint32 (sock level option)
  (with-foreign-objects ((vbuf :uint32)
                         (vlen :uint32))
    (setf (mem-aref vlen :uint32) 4)
    (let ((sts (%getsockopt sock 
                            level
                            option
                            vbuf
                            vlen)))
      (if (= sts +socket-error+)
          (get-last-error)
          (mem-aref vbuf :uint32)))))


(defmethod socket-option (sock (level (eql :socket)) (option (eql :acceptconn)))
  (get-socket-option-boolean sock +sol-socket+ +so-acceptconn+))

(defmethod socket-option (sock (level (eql :socket)) (option (eql :broadcast)))
  (get-socket-option-boolean sock +sol-socket+ +so-broadcast+))

(defmethod socket-option (sock (level (eql :socket)) (option (eql :reuseaddr)))
  (get-socket-option-boolean sock +sol-socket+ +so-reuseaddr+))

(defmethod socket-option (sock (level (eql :socket)) (option (eql :sndbuf)))
  (get-socket-option-int32 sock +sol-socket+ +so-sndbuf+))

(defmethod socket-option (sock (level (eql :socket)) (option (eql :rcvbuf)))
  (get-socket-option-int32 sock +sol-socket+ +so-rcvbuf+))

(defmethod socket-option (sock (level (eql :socket)) (option (eql :error)))
  (get-socket-option-int32 sock +sol-socket+ +so-error+))


#-(or win32 windows)
(defcstruct timeval
  (tv-sec #+(or amd64 x86-64 x64):uint64
	  #-(or amd64 x86-64 x64):uint32)
  (tv-usec :uint64))

(defmethod socket-option (sock (level (eql :socket)) (option (eql :rcvtimeo)))
  #+(or win32 windows)
  (get-socket-option-int32 sock +sol-socket +so-rcvtimeo+)
  #-(or win32 windows)
  (with-foreign-objects ((tv '(:struct timeval))
			 (len :uint32))
    (setf (mem-aref len :uint32) (foreign-type-size '(:struct timeval)))
    (let ((sts (%getsockopt sock +sol-socket+ +so-rcvtimeo+ tv len)))
      (when (= sts +socket-error+) (get-last-error)))

    (+ (* (foreign-slot-value tv '(:struct timeval) 'tv-sec) 1000)
       (truncate (foreign-slot-value tv '(:struct timeval) 'tv-usec) 1000))))

;; ------------------------------------------------
;; setting socket options

(defun set-socket-option-boolean (sock level option value)
  (with-foreign-object (vbuf :uint8 4)
    (setf (mem-aref vbuf :boolean) value)
    (let ((sts (%setsockopt sock 
                            level
                            option
                            vbuf
                            4)))
      (if (= sts +socket-error+)
          (get-last-error)
          nil))))

(defun set-socket-option-int32 (sock level option value)
  (with-foreign-object (vbuf :int32)
    (setf (mem-aref vbuf :int32) value)
    (let ((sts (%setsockopt sock 
                            level
                            option
                            vbuf
                            4)))
      (if (= sts +socket-error+)
          (get-last-error)
          nil))))

(defun set-socket-option-uint32 (sock level option value)
  (with-foreign-object (vbuf :uint32)
    (setf (mem-aref vbuf :uint32) value)
    (let ((sts (%setsockopt sock 
                            level
                            option
                            vbuf
                            4)))
      (if (= sts +socket-error+)
          (get-last-error)
          nil))))

(defun set-socket-option-pointer (sock level option pointer len)
  (let ((sts (%setsockopt sock 
                          level
                          option
                          pointer 
                          len)))
    (if (= sts +socket-error+)
        (get-last-error *errno*)
	nil)))

(defmethod (setf socket-option) (value sock (level (eql :socket)) (option (eql :acceptconn)))
  (set-socket-option-boolean sock +sol-socket+ +so-acceptconn+ value))

(defmethod (setf socket-option) (value sock (level (eql :socket)) (option (eql :broadcast)))
  (set-socket-option-boolean sock +sol-socket+ +so-broadcast+ value))

(defmethod (setf socket-option) (value sock (level (eql :socket)) (option (eql :reuseaddr)))
  (set-socket-option-boolean sock +sol-socket+ +so-reuseaddr+ value))

(defmethod (setf socket-option) (value sock (level (eql :socket)) (option (eql :sndbuf)))
  (set-socket-option-int32 sock +sol-socket+ +so-sndbuf+ value))

(defmethod (setf socket-option) (value sock (level (eql :socket)) (option (eql :rcvbuf)))
  (set-socket-option-int32 sock +sol-socket+ +so-rcvbuf+ value))

(defmethod (setf socket-option) (value sock (level (eql :socket)) (option (eql :sndtimeo)))
  (set-socket-option-int32 sock +sol-socket+ +so-sndtimeo+ value))

(defmethod (setf socket-option) (value sock (level (eql :socket)) (option (eql :rcvtimeo)))
  #+(or win32 windows)(set-socket-option-int32 sock +sol-socket+ +so-rcvtimeo+ value)
  #-(or win32 windows)
  (with-foreign-object (tv '(:struct timeval))
    (setf (foreign-slot-value tv '(:struct timeval) 'tv-sec) (truncate value 1000)
	  (foreign-slot-value tv '(:struct timeval) 'tv-usec) (mod (* value 1000) 1000000))
    (set-socket-option-pointer sock +sol-socket+ +so-rcvtimeo+ tv (foreign-type-size '(:struct timeval)))))


;; struct group_req {
;;     ULONG gr_interface;         // Interface index.
;;     SOCKADDR_STORAGE gr_group;  // Multicast address.
;; }
(defcstruct group-req
  (iface :uint32)
  #+(or amd64 x86-64 x64)(padding1 :uint32)
  (addr :uint8 :count #x84)) ;; sockaddr_storage is quite large 

(defmethod (setf socket-option) (value sock (level (eql :ip)) (option (eql :mcast-join-group)))
  "VALUE ::= (index address) where INDEX is the interface index and ADDRESS is the multicast address to join."
  (destructuring-bind (index addr) value
    (with-foreign-object (req '(:struct group-req))
      ;; memset( &req, 0, sizeof(req) );
      (dotimes (i (foreign-type-size '(:struct group-req)))
        (setf (mem-aref req :uint8 i) 0))
      
      (setf (foreign-slot-value req '(:struct group-req) 'iface) index)
      (let ((p (foreign-slot-pointer req '(:struct group-req) 'addr)))
        (etypecase addr
          (sockaddr-in
           (setf (mem-aref p '(:struct sockaddr-in)) addr))
          (sockaddr-in6
           (setf (mem-aref p '(:struct sockaddr-in6)) addr))))
      (let ((sts 
             (%setsockopt sock
                          +ipproto-ip+
                          +mcast-join-group+
                          req
                          (foreign-type-size '(:struct group-req)))))
        (if (= sts +socket-error+)
            (get-last-error)
            nil)))))

(defmethod (setf socket-option) (value sock (level (eql :ip)) (option (eql :ip-multicast-ttl)))
  "Set the mutlicast TTL. Value is an integer."
  (set-socket-option-int32 sock +ipproto-ip+ +ip-multicast-ttl+ value))

(defmethod socket-option (sock (level (eql :ip)) (option (eql :ip-multicast-if)))
  ;; returns the default interface index in host-byte order 
  (get-socket-option-uint32 sock +ipproto-ip+ +ip-multicast-if+))

(defmethod (setf socket-option) (value sock (level (eql :ip)) (option (eql :ip-multicast-if)))
  "Set the local interface to use. VALUE is a 4-octet array indicating the local address."
  (with-foreign-object (vbuf :int32)
    ;; write the address into the buffer as a 32-bit big-endian integer
    (dotimes (i 4)
      (setf (mem-ref vbuf :uint8 i) (aref value i)))
    (let ((sts (%setsockopt sock 
                            +ipproto-ip+
                            +ip-multicast-if+
                            vbuf
                            4)))
      (if (= sts +socket-error+)
          (get-last-error)
          nil))))
  
(defmethod (setf socket-option) (value sock (level (eql :ip)) (option (eql :ip-multicast-loop)))
  "Set to use or not use loopback. Value is a boolean"
  (set-socket-option-boolean sock +ipproto-ip+ +ip-multicast-loop+ value))

(defmethod socket-option (sock (level (eql :tcp)) (option (eql :nodelay)))
  (not (zerop (get-socket-option-int32 sock +ipproto-tcp+ +tcp-nodelay+))))

(defmethod (setf socket-option) (value sock (level (eql :tcp)) (option (eql :nodelay)))
  (set-socket-option-int32 sock +ipproto-tcp+ +tcp-nodelay+ (if value 1 0)))


(defun multicast-join (sock mcaddr &optional adapters)
  "Join the IPv4 multicast group on all ETHERNET interfaces."
  (declare (type sockaddr-in mcaddr))
  (let ((ads (or adapters
		 (mapcan (lambda (ad)
			   (when (and (eq (adapter-type ad) :ethernet)
				      (eq (adapter-status ad) :up))
			     (list ad)))
			 (list-adapters)))))
    (dolist (ad ads)
      ;; tell the interface to join the group                  
      (setf (socket-option sock :ip :mcast-join-group)
	    ;; argument is (ifindex mcaddr)
	    (list (adapter-index ad)
		  mcaddr)))))
  

(defun open-multicast-socket (adapter &key (ttl 2) loopback)
  "Open a socket to be used to send IPv4 multicast datagrams.
ADAPTER ::= adapter to use as originating interface.
TTL ::= integer specifying multicast ttl.
LOOPBACK ::= if true, will receive datagrams sent from loopback device, if false disables loopback.

Returns the unbound socket."
  (declare (type adapter adapter))
  (let ((sock (open-socket :type :datagram)))
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (close-socket sock)
                            ;; decline to handle the error by letting control pass through
                            nil)))
      
      ;; disable on loopback
      (setf (socket-option sock :ip :ip-multicast-loop) loopback)

      ;; select interface to send on 
      (setf (socket-option sock :ip :ip-multicast-if)
            (sockaddr-in-addr (car (adapter-unicast adapter))))

      ;; set multicast ttl
      (setf (socket-option sock :ip :ip-multicast-ttl) ttl)

      sock)))


(defun open-udp-socket (&optional port)
  "Open a UDP socket bound to local port PORT, defaults to wildcard port."
  (let ((fd (open-socket)))
    (socket-bind fd (sockaddr-in nil port))
    fd))

(defun open-tcp-socket (&optional port)
  "Open a listening TCP socket bound to local port PORT, defaults to wildcard port."
  (let ((fd (open-socket :type :stream)))
    (socket-bind fd (sockaddr-in nil port))
    (socket-listen fd)
    fd))

(defun open-tcp-connection (addr)
  "Open a TCP connection to ADDR."
  (declare (type sockaddr-in addr))
  (let ((fd (open-socket :type :stream)))
    (socket-bind fd (sockaddr-in))
    (socket-connect fd addr)
    fd))
