;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file defines all the stuff for posix platforms (Linux,FreeBSD, maybe OSX one day)

(in-package #:fsocket)

;; --------------------------------------

(defcfun (strerror "strerror") :string
  (sts :int32))

(defcvar (*errno* "errno") :int32)

(define-condition posix-error (fsocket-error)
  ((code :initform 0 :initarg :code :reader posix-error-code))
  (:report (lambda (condition stream)
             (format stream "FSOCKET-ERROR ~A (0x~X): ~A" 
                     (posix-error-code condition)
                     (posix-error-code condition)
                     (strerror (posix-error-code condition))))))

(defun get-last-error (&optional ecode)
  (let ((code (or ecode *errno*)))
    (error 'posix-error :code code)))

(defconstant +socket-error+ -1)
(defconstant +syscall-error+ -1)

(defmacro with-syscall-else-error (syscall error)
  (let ((sts (gensym)))
    `(let ((,sts ,syscall))
       (if (= ,sts ,error)
	   (get-last-error)	   
	   ,sts))))
;; --------------------------------------

(defctype canid_t :uint32) 
(defctype sa_family_t :uint16)
(defctype socklen_t :uint32) 

;; struct can_frame {
;;     canid_t can_id;  /* 32 bit CAN_ID + EFF/RTR/ERR flags */
;;     __u8    can_dlc; /* frame payload length in byte (0 .. 8) */
;;     __u8    __pad;   /* padding */
;;     __u8    __res0;  /* reserved / padding */
;;     __u8    __res1;  /* reserved / padding */
;;     __u8    data[8] __attribute__((aligned(8)));
;; };

(defcstruct can-frame
  (can_id canid_t)
  (can_dlc :uint8)
  (pad :uint8)
  (res0 :uint8)
  (res1 :uint8)
  (data :pointer))

(defcstruct timeval
  (tv_sec :int64)
  (tv_usec :int64))

(defcstruct TP
  (rx_id canid_t)
  (tx_id canid_t))
  
(defcunion can_address
  (tp (:struct TP)))

;; struct sockaddr_can {
;;     sa_family_t can_family;
;;     int         can_ifindex;
;;     union {
;; 	/* transport protocol class address info (e.g. ISOTP) */
;; 	struct { canid_t rx_id, tx_id; } tp;

;; 	/* reserved for future CAN protocols address information */
;;     } can_addr;
;; };

(defcstruct sockaddr-can
  (can_family sa_family_t)
  (can_ifindex :int)
  (can_addr (:union can_address)))


;; struct ifreq
;;   {
;; # define IFHWADDRLEN    6
;; # define IFNAMSIZ       IF_NAMESIZE
;;     union
;;       {
;;         char ifrn_name[IFNAMSIZ];       /* Interface name, e.g. "en0".  */
;;       } ifr_ifrn;

;;     union
;;       {
;;         struct sockaddr ifru_addr;
;;         struct sockaddr ifru_dstaddr;
;;         struct sockaddr ifru_broadaddr;
;;         struct sockaddr ifru_netmask;
;;         struct sockaddr ifru_hwaddr;
;;         short int ifru_flags;
;;         int ifru_ivalue;
;;         int ifru_mtu;
;;         struct ifmap ifru_map;
;;         char ifru_slave[IFNAMSIZ];      /* Just fits the size */
;;         char ifru_newname[IFNAMSIZ];
;;         __caddr_t ifru_data;
;;       } ifr_ifru;
;;   };


;; struct ifmap {
;;     unsigned long   mem_start;
;;     unsigned long   mem_end;
;;     unsigned short  base_addr;
;;     unsigned char   irq;
;;     unsigned char   dma;
;;     unsigned char   port;
;; };

(defcstruct ifmap
  (mem_start :ulong)
  (mem_end :ulong)
  (base_addr :ushort)
  (irq :uchar)
  (dma :uchar)
  (port :uchar))

(defcunion ifreq-data
  (addr (:struct sockaddr-in))
  (dst_addr (:struct sockaddr-in))
  (broadaddr (:struct sockaddr-in))
  (netmask (:struct sockaddr-in))
  (hwaddr (:struct sockaddr-in))
  (flags :uint16)
  (ifindex :int32)
  (metric :int32)
  (mtu :int32)
  (map (:struct ifmap))
  (slave :char)
  (newname :char)
  (data :pointer))

(defcstruct ifreq
  (name :uint8 :count 16)
  (data (:union ifreq-data)))



;; For when we need to allocate a bit of memory to receive addresses 
(defconstant +sockaddr-storage+ 128) 

(defun invalid-socket-p (sock)
  (= sock -1))

;; int socket(int socket_family, int socket_type, int protocol);
(defcfun (%socket "socket") :int32
  (family :int32)
  (type :int32)
  (prot :int32))

(defun open-socket (&key (family :inet) (type :datagram) protocol)
    "Open a socket. Call CLOSE-SOCKET to free resources.
FAMILY ::= address family integer. Either :INET, :INET6, :UNIX or :CAN.
TYPE ::= socket type name, defaults to SOCK_DGRAM. Can be :datagram, :stream or :raw.
PROTOCOL ::= socket protocol integer. Usually doesn't need to be specified.

Returns the socket file descriptor."
    (declare (type symbol family type)
             (type (or null integer) protocol))    
    (let ((fd (%socket (ecase family
                        (:inet +af-inet+)
                         (:inet6 +af-inet6+)
                         (:unix +af-unix+)
			 (:can +pf-can+))
		       
		       (ecase type
			 (:raw +sock-raw+)
			 (:stream +sock-stream+)
			 (:datagram +sock-dgram+))

		       (or
			(when (eql family :can)
			  #-linux (error "CAN networking only supported on Linux")
			  #+linux +can-raw+)
			protocol
			0))))
      
    (if (invalid-socket-p fd)
        (get-last-error)
        fd)))

;; int close(int fd);
(defcfun (%close "close") :int
  (fd :int32))

(defun close-socket (fd)
  "Close the socket named by FD."
  (with-syscall-else-error (%close fd) +socket-error+))

;; int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
(defcfun (%bind "bind") :int32
  (fd :int32)
  (addr :pointer)
  (len :int32))

(defun socket-bind (fd addr)
  "Bind the socket to the local address/interface.
FD ::= socket file descriptor.
ADDR ::= local address or can interface. Can be SOCKADDR-IN, SOCKADDR-IN6 or string for internet sockets; must be of type CAN-INTERFACE for can sockets"
  (let ((sock-addr-pointer NIL)
	(sock-addr-len NIL))
    (if (can-interface-p addr)
	;; bind can socket
	(with-foreign-object (ifr '(:struct ifreq))
	  (with-foreign-object (sockaddr '(:struct sockaddr-can))
	    (let* ((n (can-interface-name addr))
		   (n-size (length n)))
	      (if (string= "any" n)				  
		  (setf (foreign-slot-value sockaddr '(:struct sockaddr-can) 'can_ifindex) 0)
		  (progn
		    (lisp-string-to-foreign n (foreign-slot-value ifr '(:struct ifreq) 'name) (1+ n-size))
		    (with-syscall-else-error (%ioctl fd +siocgifindex+ ifr) +syscall-error+)
		    (setf (foreign-slot-value sockaddr '(:struct sockaddr-can) 'can_family) +pf-can+)
		    (let* ((ifrdata (foreign-slot-value ifr '(:struct ifreq) 'data))
			   (index (foreign-slot-value ifrdata '(:union ifreq-data) 'ifindex)))      
		      (setf (foreign-slot-value sockaddr '(:struct sockaddr-can) 'can_ifindex) index))))	      
	      (setq sock-addr-pointer sockaddr
		    sock-addr-len (foreign-type-size '(:struct sockaddr-in))))))		    	    
	;; bind internet socket
	(with-foreign-object (p :uint8 +sockaddr-storage+)
	  (let ((len +sockaddr-storage+))
	    (etypecase addr
	      (sockaddr-in
	       (setf (mem-aref p '(:struct sockaddr-in)) addr
		     len (foreign-type-size '(:struct sockaddr-in))))
	      (sockaddr-in6
	       (setf (mem-aref p '(:struct sockaddr-in6)) addr
		     len (foreign-type-size '(:struct sockaddr-in6))))
	      (string
	       (setf (mem-aref p '(:struct sockaddr-un)) addr
		     len (foreign-type-size '(:struct sockaddr-un)))))
	    (setq sock-addr-pointer p
		  sock-addr-len len))))	 

    (with-syscall-else-error
	(%bind fd sock-addr-pointer sock-addr-len) +socket-error+)))
      

 ;; int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
(defcfun (%connect "connect") :int32
  (fd :int32)
  (addr :pointer)
  (len :int32))

#+linux(defconstant +ewouldblock+ 11)
#+(or freebsd darwin)(defconstant +ewouldblock+ 35)

#+linux(defconstant +einprogress+ 115)
#+(or freebsd darwin)(defconstant +einprogress+ 36)

(defun socket-connect (fd addr)
    "Connect the socket to the remote address.
SOCK :: socket.
ADDR ::= remote address.

Returns true if the operation completed successfully.
If the socket is in non-blocking mode and the operation would block returns NIL. 
A :POLLOUT event indicates a subsequent socket-connect will complete immediately."
    (with-foreign-object (a :uint8 +sockaddr-storage+)
      (let ((len +sockaddr-storage+))
        (etypecase addr
          (sockaddr-in
           (setf (mem-aref a '(:struct sockaddr-in)) addr
                 len (foreign-type-size '(:struct sockaddr-in))))
          (sockaddr-in6
           (setf (mem-aref a '(:struct sockaddr-in6)) addr
                 len (foreign-type-size '(:struct sockaddr-in6))))
          (string 
           (setf (mem-aref a '(:struct sockaddr-un)) addr
                 len (foreign-type-size '(:struct sockaddr-un)))))
        (let ((sts (%connect fd a len)))
          (if (= sts +socket-error+)
              (let ((ecode *errno*))
                (if (or (= ecode +einprogress+) (= ecode +ewouldblock+))
                    nil
                    (get-last-error ecode)))
              t)))))

;; int listen(int sockfd, int backlog);
(defcfun (%listen "listen") :int32
  (fd :int32)
  (backlog :int32))

(defun socket-listen (fd &optional backlog)
  "Start the socket listening."
  (with-syscall-else-error (%listen fd (or backlog 128)) +socket-error+)) ;; SOMAXCONN
                      
;; int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
(defcfun (%accept "accept") :int32
  (fd :int32)
  (addr :pointer)
  (len :pointer))

(defun socket-accept (fd)
  "Accept a connection from the listening socket.
SOCK ::= listening socket.

Returns (values conn addr) where
CONN ::= new connected socket.
ADDR ::= address of the connected socket.

If the socket is in non-blocking mode and the operation would block returns nil.
A :POLLIN event indicates a subsequent socket-accept will complete immediately."
  (with-foreign-objects ((buffer :uint8 +sockaddr-storage+)
                         (alen :uint32))
    (setf (mem-aref alen :uint32) +sockaddr-storage+)
    (let ((sts (%accept fd buffer alen)))
      (cond
        ((invalid-socket-p sts)
         (let ((ecode *errno*))
           (if (or (= ecode +einprogress+) (= ecode +ewouldblock+))
               nil
               (get-last-error ecode))))
        (t
         (handler-bind ((error (lambda (e)
                                 (declare (ignore e))
                                 (close-socket sts)
                                 nil)))
           (let ((addr (translate-sockaddr-from-foreign buffer)))
             (values sts addr))))))))

;; int shutdown(int sockfd, int how);
(defcfun (%shutdown-socket "shutdown") :int32
  (fd :int32)
  (how :int32))

(defun socket-shutdown (fd &optional (how :receive))
  "Shutdown traffic on the TCP socket.
HOW ::= :SEND to stop sending, :RECEIVE to stop receiving, :BOTH to stop both."
  (with-syscall-else-error      
      (%shutdown-socket fd
			(ecase how
			  (:send 1)
			  (:both 2)
			  (:receive 0)))
    +socket-error+))
    
;; ------------------------------------------------

(defctype size-t
    #+(or amd64 x86-64 x64):uint64
    #-(or amd64 x86-64 x64):uint32)
(defctype ssize-t
    #+(or amd64 x86-64 x64):int64
    #-(or amd64 x86-64 x64):int32)

;; ssize_t send(int sockfd, const void *buf, size_t len, int flags);
(defcfun (%send "send") ssize-t
  (fd :int32)
  (buffer :pointer)
  (len size-t)
  (flags :int32))

(declaim (ftype (function (t (or (vector (unsigned-byte 8)) foreign-pointer)
			     &key (:start integer) (:end (or null integer)))
			  integer)))
(defun socket-send (fd buffer &key (start 0) end)
  "Send a buffer on the connected socket.
SOCK ::= connected socket.
BUFFER ::= octet vector, foreign pointer or can-frame.
START ::= start index of buffer.
END ::= end index of buffer.

Returns the number of bytes actually sent, which can be less than the requested length."
  (declare (type (or (vector (unsigned-byte 8)) foreign-pointer can-packet) buffer))
  (with-syscall-else-error
      (etypecase buffer 
	((vector (unsigned-byte 8))
	 (let ((count (- (or end (length buffer)) start)))
	   (with-foreign-object (p :uint8 count)
	     (dotimes (i count)
	       (setf (mem-aref p :uint8 i) (aref buffer (+ start i))))
	     (%send fd p count 0))))      
	(foreign-pointer
	 (let ((count (- (or end (error "Must provide end when passing pointer"))
			 start)))
	   (%send fd (inc-pointer buffer start) count 0)))
	(can-packet
	 (let* ((id (can-packet-id buffer))
		(payload (can-packet-data buffer))
		(payload-size (length payload)))
	   (with-foreign-object (frame '(:struct can-frame))  
	     (setf (foreign-slot-value frame '(:struct can-frame) 'can_id) id)
	     (setf (foreign-slot-value frame '(:struct can-frame) 'can_dlc) payload-size)
	     (let ((ptr (foreign-slot-pointer frame '(:struct can-frame) 'data)))	       
	       (lisp-array-to-foreign payload ptr `(:array :uint8 ,payload-size)))
	     (%write fd frame (foreign-type-size '(:struct can-frame)))))))
    +socket-error+))
	 
;; ssize_t sendto(int sockfd, const void *buf, size_t len, int flags,
;;                const struct sockaddr *dest_addr, socklen_t addrlen);
(defcfun (%sendto "sendto") ssize-t
  (fd :int32)
  (buffer :pointer)
  (len size-t)
  (flags :int32)
  (addr :pointer)
  (alen :int32))

(declaim (ftype (function (t (or (vector (unsigned-byte 8)) foreign-pointer) (or sockaddr-in sockaddr-in6) &key (:start integer) (:end (or null integer)))
			  integer)))
(defun socket-sendto (fd buffer addr &key (start 0) end)
  "Send data to the address on the socket.
FD ::= socket.
ADDR ::= local address or can interface. Can be SOCKADDR-IN, SOCKADDR-IN6 or string for internet sockets must be of type CAN-INTERFACE for can sockets.
ADDR ::= destination address, either a SOCKADDR-IN or SOCKADDR-IN6 structure or a can-interface.
START ::= buffer start index.
END ::= buffer end index.

Returns the number of octets actually sent, which can be less than the number requested."
  (declare (type (or (vector (unsigned-byte 8)) foreign-pointer can-packet) buffer))
  (with-syscall-else-error
      (etypecase buffer
	((vector (unsigned-byte 8))
	 (let ((count (- (or end (length buffer)) start))
	       (alen 0))
	   (with-foreign-objects ((p :uint8 count)
				  (a :uint8 +sockaddr-storage+))
	     (dotimes (i count)
	       (setf (mem-aref p :uint8 i)
		     (aref buffer (+ start i))))
	     (etypecase addr
	       (sockaddr-in
		(setf (mem-aref a '(:struct sockaddr-in)) addr
		      alen (foreign-type-size '(:struct sockaddr-in))))
	       (sockaddr-in6
		(setf (mem-aref a '(:struct sockaddr-in6)) addr
		      alen (foreign-type-size '(:struct sockaddr-in6))))
	       (string 
		(setf (mem-aref a '(:struct sockaddr-un)) addr
		      alen (foreign-type-size '(:struct sockaddr-un)))))
	     (%sendto fd p count 0 a alen))))	   
	
	(foreign-pointer
	 (let ((alen 0)
	       (count (- (or end (error "Must provide end when passing pointer"))
			 start)))
	   (with-foreign-object (a :uint8 +sockaddr-storage+)
	     (etypecase addr
	       (sockaddr-in (setf (mem-aref a '(:struct sockaddr-in)) addr
				  alen (foreign-type-size '(:struct sockaddr-in))))
	       (sockaddr-in6
		(setf (mem-aref a '(:struct sockaddr-in6)) addr
		      alen (foreign-type-size '(:struct sockaddr-in6)))))
	     (%sendto fd (inc-pointer buffer start) count 0 a alen))))
	
	(can-packet
	 ;; todo: avoid copy-paste
	 (let* ((id (can-packet-id buffer))
		(payload (can-packet-data buffer))
		(payload-size (length payload))
		(n (can-interface-name addr))
		(n-size (length n)))       
	   (with-foreign-object (ifr '(:struct ifreq))
	     (with-foreign-object (sockaddr '(:struct sockaddr-can))
	       (with-foreign-object (frame '(:struct can-frame))
		 (lisp-string-to-foreign n (foreign-slot-value ifr '(:struct ifreq) 'name) (1+ n-size))
		 (with-syscall-else-error (%ioctl fd +siocgifindex+ ifr) +syscall-error+)
		 (setf (foreign-slot-value sockaddr '(:struct sockaddr-can) 'can_family) +pf-can+)
		 (let* ((ifrdata (foreign-slot-value ifr '(:struct ifreq) 'data))
			(index (foreign-slot-value ifrdata '(:union ifreq-data) 'ifindex)))      
		   (setf (foreign-slot-value sockaddr '(:struct sockaddr-can) 'can_ifindex) index))
		 (setf (foreign-slot-value frame '(:struct can-frame) 'can_id) id)
		 (setf (foreign-slot-value frame '(:struct can-frame) 'can_dlc) payload-size)
		 (let ((ptr (foreign-slot-pointer frame '(:struct can-frame) 'data)))	       
		   (lisp-array-to-foreign payload ptr `(:array :uint8 ,payload-size))		   
		       (%sendto
			fd
			frame
			(foreign-type-size '(:struct can-frame))
			0
			sockaddr
			(foreign-type-size '(:struct can-frame))))))))))
    +socket-error+))

;; ssize_t recv(int sockfd, void *buf, size_t len, int flags);
(defcfun (%recv "recv") ssize-t
  (fd :int32)
  (buffer :pointer)
  (len size-t)
  (flags :int32))

(declaim (ftype (function (t (or (vector (unsigned-byte 8)) foreign-pointer)
			     &key (:start integer) (:end (or null integer)))
			  integer)))
(defun socket-recv (fd buffer &key (start 0) end)
  "Receive data from the socket.
FD ::= socket.
BUFFER ::= octet vector, foreign pointer or can-frame that receives data.
START ::= buffer start index.
END ::= buffer end index.

Retuns the number of bytes actually received, which can be less than the number requested."
  (declare (type (or (vector (unsigned-byte 8)) foreign-pointer can-packet) buffer))
  (etypecase buffer
    ((vector (unsigned-byte 8))
     (let ((count (- (or end (length buffer)) start)))
       (with-foreign-object (p :uint8 count)
	 (let ((sts (with-syscall-else-error (%recv fd p count 0) +socket-error+)))		 
	   (dotimes (i sts)
	     (setf (aref buffer (+ start i))
		   (mem-aref p :uint8 i)))
	   sts))))
    (foreign-pointer
     (let ((count (- (or end (error "Must provide end when passing pointer")) start)))
       (with-syscall-else-error (%recv fd 
			 (inc-pointer buffer start)
			 count
			 0)
	 +socket-error+)))
			 
    (can-packet
     (with-foreign-object (frame '(:struct can-frame))
       (let ((sts (with-syscall-else-error
		      (%read fd frame (foreign-type-size '(:struct can-frame)))
		    +socket-error+)))	 
	 (with-foreign-slots ((can_id can_dlc data) frame (:struct can-frame))
	   (let* ((ptr-data (foreign-slot-pointer frame '(:struct can-frame) 'data))
		  (data (foreign-array-to-lisp ptr-data `(:array :int8 ,can_dlc))))
	     (setf (can-packet-id buffer) can_id
		   (can-packet-data buffer) data)
	     (with-foreign-object (tv '(:struct timeval))
	       (with-syscall-else-error (%ioctl fd +siocgstamp+ tv) +syscall-error+)
	       (with-foreign-slots ((tv_sec tv_usec) tv (:struct timeval))
		 (setf (can-packet-timestamp buffer) (list tv_sec tv_usec))))))
	 sts)))))

;;ssize_t recvfrom(int sockfd, void *buf, size_t len, int flags,
;;                 struct sockaddr *src_addr, socklen_t *addrlen);
(defcfun (%recvfrom "recvfrom") ssize-t
  (fd :int32)
  (buffer :pointer)
  (len size-t)
  (flags :int32)
  (addr :pointer)
  (alen :pointer))

(declaim (ftype (function (t (or (vector (unsigned-byte 8)) foreign-pointer)
			     &key (:start integer) (:end (or null integer)))
			  (values integer (or sockaddr-in sockaddr-in6)))))
(defun socket-recvfrom (fd buffer &key (start 0) end)
  "Receive data from the socket.
FD ::= socket.
BUFFER ::= octet vector or foreign pointer.
START ::= start index.
END ::= end index.

Returns (values count addr) where
COUNT ::= number of octets actually received, which can be less tha nthe number requested.
ADDR ::= remote address or can-interface from which the data was received.
"
  (declare (type (or (vector (unsigned-byte 8)) foreign-pointer can-packet) buffer))
  (etypecase buffer
    ((vector (unsigned-byte 8))
     (let ((count (- (or end (length buffer)) start)))
       (with-foreign-objects ((p :uint8 count)
			      (a :uint8 +sockaddr-storage+)
			      (alen :uint32))
	 (setf (mem-aref alen :uint32) +sockaddr-storage+)
	 (let ((sts (with-syscall-else-error (%recvfrom fd p count 0 a alen) +socket-error+)))	   
	      (dotimes (i sts)
		(setf (aref buffer (+ start i)) (mem-ref p :uint8 i)))
	      (let ((addr (translate-sockaddr-from-foreign a)))
		(values sts addr))))))
    (foreign-pointer
     (let ((count (- (or end (error "Must provide end when passing pointer"))
		     start)))
       (with-foreign-objects ((a :uint8 +sockaddr-storage+)
			      (alen :uint32))
	 (setf (mem-aref alen :uint32) +sockaddr-storage+)
	 (let ((sts (with-syscall-else-error
			(%recvfrom fd (inc-pointer buffer start) count 0 a alen) +socket-error+)))
	   (values sts (translate-sockaddr-from-foreign a))))))
    (can-packet
     (with-foreign-object (frame '(:struct can-frame))
       (with-foreign-object (sockaddr '(:struct sockaddr-can))
	 (with-foreign-object (len 'socklen_t)	   
	   (setf (mem-aref len 'socklen_t) (foreign-type-size '(:struct sockaddr-can)))
	   (let ((sts (with-syscall-else-error
			  (%recvfrom
			   fd frame (foreign-type-size '(:struct can-frame))
			   0 sockaddr len) +socket-error+)))
	     ;; todo: make function to avoid copy-paste
	     (with-foreign-slots ((can_id can_dlc data) frame (:struct can-frame))
	       (let* ((ptr-data (foreign-slot-pointer frame '(:struct can-frame) 'data))
		      (data (foreign-array-to-lisp ptr-data `(:array :int8 ,can_dlc))))
		 (setf (can-packet-id buffer) can_id
		       (can-packet-data buffer) data)
		 (with-foreign-object (tv '(:struct timeval))
		   (with-syscall-else-error (%ioctl fd +siocgstamp+ tv) +syscall-error+)
		   (with-foreign-slots ((tv_sec tv_usec) tv (:struct timeval))
		     (setf (can-packet-timestamp buffer) (list tv_sec tv_usec)))
		   (with-foreign-object (ifr '(:struct ifreq))
		     (let* ((ifrdata (foreign-slot-value ifr '(:struct ifreq) 'data)))
		       (setf (foreign-slot-value ifrdata '(:union ifreq-data) 'ifindex)
			     (foreign-slot-value sockaddr '(:struct sockaddr-can) 'can_ifindex))
		       (with-syscall-else-error (%ioctl fd +siocgifname+ ifr) +socket-error+)
		       (setf (can-packet-origin buffer)
			     (foreign-string-to-lisp (foreign-slot-value ifr '(:struct ifreq) 'name)))
		       (values sts (foreign-slot-value ifr '(:struct ifreq) 'name))))))))))))))


(defmacro with-can-socket ((socket &optional (interface-name "any")) &rest body)
  `(let ((,socket (open-socket :family :can :type :raw)))     
     (socket-bind ,socket (make-can-interface :name ,interface-name))
     (unwind-protect	  
	  (progn ,@body)
       (close-socket ,socket))))

;; int getsockname(int socket, struct sockaddr *restrict address, socklen_t *restrict address_len);
(defcfun (%getsockname "getsockname") :int32
  (fd :int32)
  (addr :pointer)
  (len :pointer))
(defun socket-name (fd)
    "Get the address to which the socket has been bound.
FD ::= socket as returned from OPEN-SOCKET.
Returns a SOCKADDR-IN or SOCKADDR-IN6 structure."
  (with-foreign-objects ((addr :uint32 +sockaddr-storage+)
                         (len :uint32))
    (setf (mem-aref len :uint32) +sockaddr-storage+)
    (with-syscall-else-error (%getsockname fd addr len) +socket-error+)
    (translate-sockaddr-from-foreign addr)))

;;int getpeername(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
(defcfun (%getpeername "getpeername") :int32
  (fd :int32)
  (addr :pointer)
  (len :pointer))

(defun socket-peer (fd)
  (with-foreign-objects ((addr :uint32 +sockaddr-storage+)
                         (len :uint32))
    (setf (mem-aref len :uint32) +sockaddr-storage+)
    (with-syscall-else-error (%getpeername fd addr len) +socket-error+)
    (translate-sockaddr-from-foreign addr)))
  

;; int getsockopt(int sockfd, int level, int optname, void *optval, socklen_t *optlen);
(defcfun (%getsockopt "getsockopt") :int32
  (fd :int32)
  (level :int32)
  (option :int32)
  (val :pointer)
  (vlen :pointer))

;;int setsockopt(int sockfd, int level, int optname, const void *optval, socklen_t optlen);
(defcfun (%setsockopt "setsockopt") :int32
  (fd :int32)
  (level :int32)
  (option :int32)
  (val :pointer)
  (vlen :int32))

;; int fcntl(int fd, int cmd, ... /* arg */ );
(defcfun (%fcntl "fcntl") :int32
  (fd :int32)
  (cmd :int32)
  (arg :pointer))

#+linux(defconstant +o-nonblock+ 2048)
#+(or freebsd darwin)(defconstant +o-nonblock+ #x0004)

(defconstant +f-getfl+ 3)
(defconstant +f-setfl+ 4)

#+linux(defconstant +fionbio+ #x5421)
#+(or freebsd darwin)(defconstant +fionbio+ 2147772030)

(defun socket-flags (fd)
  (%fcntl fd +f-getfl+ (null-pointer)))

(defun (setf socket-flags) (flags fd)
  (%fcntl fd +f-setfl+ (make-pointer flags)))

(defun set-nonblocking (fd)
  (let ((flags (socket-flags fd)))
    (setf (socket-flags fd) (logior flags +o-nonblock+))))

;; -----------------------------------------------------

(defcstruct (pollfd :class pollfd-tclass)
  (fd :int32)
  (events :uint16)
  (revents :uint16))

(defmethod translate-from-foreign (ptr (type pollfd-tclass))
  (let ((pollfd (make-instance 'pollfd)))
    (setf (pollfd-fd pollfd) (foreign-slot-value ptr '(:struct pollfd) 'fd)
          (pollfd-events pollfd) (foreign-slot-value ptr '(:struct pollfd) 'events)
          (pollfd-revents pollfd) (foreign-slot-value ptr '(:struct pollfd) 'revents))
    pollfd))

(defmethod translate-into-foreign-memory (pollfd (type pollfd-tclass) ptr)
  (setf (foreign-slot-value ptr '(:struct pollfd) 'fd)
        (pollfd-fd pollfd)
        (foreign-slot-value ptr '(:struct pollfd) 'events)
        (pollfd-events pollfd)
        (foreign-slot-value ptr '(:struct pollfd) 'revents)
        (pollfd-revents pollfd))
  ptr)

(defstruct poll-context
  fds)

;; these are basically nops on POSIX.
(defun open-poll ()
  "Open a poll context."
  (make-poll-context :fds nil))

(defun close-poll (pc)
  "Close a poll context"
  (declare (ignore pc))
  nil)

(declaim (ftype (function (poll-context pollfd) pollfd) poll-register))
(defun poll-register (pc pollfd)
  "Register a pollfd descriptor with a poll context."
  (declare (type poll-context pc)
	   (type pollfd pollfd))
  (push pollfd (poll-context-fds pc))

  ;; Q: should we set the fd to be non-blocking here, to preserve semantics with Windows?
  (set-nonblocking (pollfd-fd pollfd))
  
  pollfd)

(declaim (ftype (function (poll-context pollfd) null)))
(defun poll-unregister (pc pollfd)
  "Unregister a pollfd descriptor from a poll context."
  (declare (type poll-context pc)
	   (type pollfd pollfd))

  (setf (poll-context-fds pc)
        (remove pollfd (poll-context-fds pc)))
  nil)

(defun poll-find (pc fd)
  "Lookup the pollfd for the file descriptor.
PC ::= poll context.
FD ::= file desctipror.
Returns the pollfd with the matching file descriptor if found."
  (declare (type poll-context pc))
  (find fd (poll-context-fds pc) 
        :key #'pollfd-fd 
        :test #'=))

;; int poll(struct pollfd *fds, nfds_t nfds, int timeout);
(defcfun (%poll "poll") :int32
  (fds :pointer)
  (count :int32)
  (timeout :int32))

(declaim (ftype (function (poll-context &key (:timeout (or null integer))) list) poll))
(defun poll (pc &key timeout)
  "Poll the sockets registered with the poll context for network events.

PC ::= poll context as returne from OPEN-POLL.
TIMEOUT ::= if supplied time in milliseconds to wait. If not supplied defaults to infinity.

Returns a list of registered pollfd structures. Users should check the REVENTS slot for pending events.
"
  (declare (type poll-context pc)
           (type (or null integer) timeout))
  (let* ((fds (poll-context-fds pc))
         (count (length fds)))
    (with-foreign-object (p '(:struct pollfd) count)
      (do ((%fds fds (cdr %fds))
           (i 0 (1+ i)))
          ((null %fds))
        (setf (mem-aref p '(:struct pollfd) i) (car %fds)))
      (let ((sts (%poll p count (or timeout -1))))
        (cond
          ((< sts 0)
           (error "error"))
          ((= sts 0)
           ;; timeout
           nil)
          (t
           (do ((i 0 (1+ i))
                (%fds fds (cdr %fds))
                (ret fds))
               ((null %fds) ret)
             (let ((revents (foreign-slot-value (inc-pointer p (* i (foreign-type-size '(:struct pollfd))))
                                                '(:struct pollfd) 
						'revents)))
	       (setf (pollfd-revents (car %fds)) revents)))))))))



;; --------------------------------------

;; Below we get a list of local host hardware adapters

;; struct ifaddrs {
;;                struct ifaddrs  *ifa_next;    /* Next item in list */
;;                char            *ifa_name;    /* Name of interface */
;;                unsigned int     ifa_flags;   /* Flags from SIOCGIFFLAGS */
;;                struct sockaddr *ifa_addr;    /* Address of interface */
;;                struct sockaddr *ifa_netmask; /* Netmask of interface */
;;                union {
;;                    struct sockaddr *ifu_broadaddr;
;;                                     /* Broadcast address of interface */
;;                    struct sockaddr *ifu_dstaddr;
;;                                     /* Point-to-point destination address */
;;                } ifa_ifu;
;;            #define              ifa_broadaddr ifa_ifu.ifu_broadaddr
;;            #define              ifa_dstaddr   ifa_ifu.ifu_dstaddr
;;                void            *ifa_data;    /* Address-specific data */
;;            };

(defcstruct ifaddrs
  (next :pointer)
  (name :pointer)
  (flags :uint32)
  (addr :pointer)
  (netmask :pointer)
  (broadcast :pointer)
  (data :pointer))
    
;; int getifaddrs(struct ifaddrs **ifap);
(defcfun (%getifaddrs "getifaddrs") :int32
  (ifaddrs :pointer))

;; void freeifaddrs(struct ifaddrs *ifa);
(defcfun (%freeifaddrs "freeifaddrs") :void
  (ifaddrs :pointer))
        

;; unsigned int if_nametoindex(const char *ifname);
(defcfun (%if-nametoindex "if_nametoindex") :uint32
  (name :pointer))

;; int ioctl(int fd, unsigned long request, void *argp);
(defcfun (%ioctl "ioctl") :int32
  (fd :int32)
  (req :uint32)
  (argp :pointer))

(defconstant +iff-loopback+ #x8)
(defconstant +iff-up+ 1)

#+freebsd(defconstant +arphrd-ether+ 2)
#+linux(defconstant +arphrd-ether+ 1)
#+darwin(defconstant +arphrd-ether+ 1)

#+linux(defconstant +arphrd-ppp+ 512)
#+linux(defconstant +arphrd-loopback+ 772)
#+linux(defconstant +arphrd-80211+ 801)

;;#+(or win32 windows)(defconstant +siocgifmtu+ #x8921)
#+linux(defconstant +siocgifmtu+ #x8921)
#+freebsd(defconstant +siocgifmtu+ #x8921)
#+darwin(defconstant +siocgifmtu+ #xC0206933)

#+linux(defconstant +siocgifhwaddr+ #x8927)
#+freebsd(defconstant +af-link+ #x12)
#+darwin(defconstant +af-link+ #x12)
#+(or freebsd darwin)(defconstant +af-ieee80211+ 37)

#+linux(defconstant +siocgifnetmask+ #x891b)
#+linux(defconstant +siocgifbrdaddr+ #x8919)

(defun list-adapters ()
  (let ((ret nil))
    (with-foreign-object (ifaddrs :pointer)
      (let ((sts (%getifaddrs ifaddrs)))
        ;; if something bad happened then bailout now 
        (when (= sts +socket-error+) (get-last-error))

        ;; now we getifaddrs and iterate over each of them 
        (let ((fd (open-socket)))
          (do ((ifa (mem-ref ifaddrs :pointer)
                    (foreign-slot-value ifa '(:struct ifaddrs) 'next))
               (ads nil))
              ((null-pointer-p ifa) (setf ret ads))
            (let ((ad (make-adapter)))
              (setf (adapter-name ad)
                    (foreign-string-to-lisp (foreign-slot-value ifa '(:struct ifaddrs) 'name)))
              
              (let ((flags (foreign-slot-value ifa '(:struct ifaddrs) 'flags)))
                ;; check the status
                (if (zerop (logand flags +iff-up+)) ;; IFF_UP
                    (setf (adapter-status ad) :down)
                    (setf (adapter-status ad) :up))

                ;; check if loopback
                (unless (zerop (logand flags +iff-loopback+)) ;; IFF_LOOPBACK
                  (setf (adapter-type ad) :loopback)))

              ;; get the address
              (let ((ap (foreign-slot-value ifa '(:struct ifaddrs) 'addr)))
                (let ((family #+(or freebsd darwin)(mem-ref ap :uint8 1)
                              #-(or freebsd darwin)(mem-ref ap :uint16)))
                  (cond
                    ((= family +af-inet+)
                     (let ((addr (mem-ref ap '(:struct sockaddr-in))))
                       (push addr (adapter-unicast ad))))
                    ((= family +af-inet6+)
                     (let ((addr (mem-ref ap '(:struct sockaddr-in6))))
                       (push addr (adapter-unicast ad))))
		    ;; TODO: find out how to identify PPP interfaces on FreeBSD and Darwin
                    #+(or freebsd darwin)
                    ((= family +af-link+)
                     ;; freebsd provides the physical address directly in the ifaddrs struct
                     (let ((mac (make-array 6)))
                       (dotimes (i 6)
                         (setf (aref mac i) (mem-aref ap :uint8 (+ 4 i))))
                       (setf (adapter-address ad) mac
                             (adapter-type ad) :ethernet))))))

	      ;; get the netmask and broadcast on freebsd and darwin
	      #-linux 
	      (progn
		(let ((np (foreign-slot-value ifa '(:struct ifaddrs) 'netmask)))
		  (unless (null-pointer-p np)
		    (let ((family (translate-sockaddr-family np)))
		      (when (= family +af-inet+)
			(push (translate-sockaddr-from-foreign np)
			      (adapter-netmask ad))))))
		
		;; get the broadcast
		(let ((bp (foreign-slot-value ifa '(:struct ifaddrs) 'broadcast)))
		  (unless (null-pointer-p bp)
		    (let ((family (translate-sockaddr-family bp)))
		      (when (= family +af-inet+)
			(push (translate-sockaddr-from-foreign bp)
			      (adapter-broadcast ad)))))))
	      
              ;; get the index
              (let ((index (%if-nametoindex (foreign-slot-value ifa '(:struct ifaddrs) 'name))))
                (setf (adapter-index ad) index))

              ;; get the hardware address and other ioctl data
              (with-foreign-object (ifr '(:struct ifreq))            
                ;; copy the interface name into the req struct
                (lisp-string-to-foreign (adapter-name ad) ifr 16)

                ;; get mtu
                (let ((sts (%ioctl fd
                                   +siocgifmtu+ ;; SIOCGIFMTU
                                   ifr))
                      (ifdata (foreign-slot-pointer ifr '(:struct ifreq) 'data)))
                  (unless (= sts +socket-error+)
                    (setf (adapter-mtu ad) (foreign-slot-value ifdata '(:union ifreq-data) 'mtu))))

                #+linux 
		;; have to get netmask usuing ioctl()
		(let ((sts (%ioctl fd 
				   +siocgifnetmask+
				   ifr))
		      (ifdata (foreign-slot-pointer ifr '(:struct ifreq) 'data)))
		  (unless (= sts +socket-error+) 
		    (push (translate-sockaddr-from-foreign ifdata) 
			  (adapter-netmask ad))))

		#+linux 
		;; have to get netmask usuing ioctl()
		(let ((sts (%ioctl fd 
				   +siocgifbrdaddr+
				   ifr))
		      (ifdata (foreign-slot-pointer ifr '(:struct ifreq) 'data)))
		  (unless (= sts +socket-error+) 
		    (push (translate-sockaddr-from-foreign ifdata) 
			  (adapter-broadcast ad))))

                ;; have to get hardware address on linux by calling ioctl()
                #+linux
                (let ((sts (%ioctl fd
                                   +siocgifhwaddr+ ;; SIOCGIFHWADDR
                                   ifr))
                      (ifdata (foreign-slot-pointer ifr '(:struct ifreq) 'data)))
                  (unless (= sts +socket-error+)
                    ;; get the hardware address out
                    (let ((sa (foreign-slot-pointer ifdata '(:union ifreq-data) 'addr)))
                      (let ((family (mem-ref sa :uint16))) 
                        (cond
                          ((= family +arphrd-ether+) ;; ARPHRD_ETHER
                           (let ((addr (make-array 6)))
                             (dotimes (i 6)
                               (setf (aref addr i)
                                     (mem-ref sa :uint8 (+ 2 i))))
                             (setf (adapter-address ad) addr
                                   (adapter-type ad) :ethernet)))
                          ((= family +arphrd-loopback+) ;; ARPHRD_LOOPBACK
                           (setf (adapter-type ad) :loopback))
			  ((= family +arphrd-ppp+) ;; ARPHRD_PPP
			   (setf (adapter-type ad) :ppp))
			  ((= family +arphrd-80211+) ;; ARPHRD_80211
			   (setf (adapter-type ad) :ieee80211))
			  (t
                           ;; unknown type
                           (setf (adapter-type ad) family))))))))

              (push ad ads)))
          
          ;; finally we close the dummy socket and free the interfaces
          (close-socket fd))
        (%freeifaddrs (mem-ref ifaddrs :pointer))))

    ;; ret contains a list of interfaces i.e. we have one instance per IP address
    ;; we want a list of adapters
    (do ((alist ret (cdr alist))
         (ads nil))
        ((null alist) ads)
      (let ((ad1 (car alist)))
        ;; lookup this ad in the list of return ads. if it's alrady there then merge it
        ;; otherwise we add it
        (let ((rad (find (adapter-index ad1) ads :key #'adapter-index)))
          (cond
            (rad
             ;; already there, merge
             (setf (adapter-unicast rad)
                   (append (adapter-unicast rad) (adapter-unicast ad1)))
             (when (null (adapter-address rad))
               (setf (adapter-address rad) (adapter-address ad1)))
             (when (null (adapter-type rad))
               (setf (adapter-type rad) (adapter-type ad1))))
            (t
             ;; not there yet, just push on
             (push ad1 ads))))))))

  

;; ------------------------------------------------------

;; DNS

;; Fortunately this seems to have th same layout on Linux and FreeBSD
;; struct __res_state {
;;         int     retrans;                /*%< retransmission time interval */
;;         int     retry;                  /*%< number of times to retransmit */
;;         u_long  options;                /*%< option flags - see below. */
;;         int     nscount;                /*%< number of name servers */
;;         struct sockaddr_in
;;                 nsaddr_list[MAXNS];     /*%< address of name server */
;; .... and more ... up to 512 bytes

;; NOTE:
;; on FreeBSD sizeof(struct __res_state) == 552
;; on Linux sizeof(struct __res_state) == 512

(defcstruct res-state
  (retrans :int32)
  (retry :int32)
  ;; Bugfix: u_long is 64 bits on x64 and 32 bits on x86 
  (options #-(or x64 x86-64 amd64) :uint32 #+(or x64 x86-64 amd64) :uint64)
  (nscount :int32)
  (nsaddrs (:struct sockaddr-in) :count 3)) ;; MAXNS == 3

#+darwin
(progn 
  (define-foreign-library lresolv
    (t (:default "libresolv")))
  
  (use-foreign-library lresolv)
  )

(defcfun (%res-ninit #+darwin "res_9_ninit" #-darwin "__res_ninit") :int32
  (state :pointer))

;; Bugfix: Linux doesn't have res_ndestroy so we just call res_nclose instead 
(defcfun (%res-ndestroy #+darwin "res_9_ndestroy" #+freebsd "__res_ndestroy" #+linux "__res_nclose") :void
  (state :pointer))

(defun get-name-servers ()
  "Returns a list of SOCKADDR-IN addresses for configured DNS name servers."
  (with-foreign-object (p :uint8 600) ;; allocate more than we need just in case
    ;; clear it out
    (dotimes (i 600)
      (setf (mem-aref p :uint8 i) 0))
    
    (let ((sts (%res-ninit p)))
      (unless (zerop sts) (get-last-error))
      (unwind-protect 
	   (let ((ap (foreign-slot-pointer p '(:struct res-state) 'nsaddrs)))
	     (loop :for i :below (foreign-slot-value p '(:struct res-state) 'nscount)
		:collect (mem-aref ap '(:struct sockaddr-in) i)))
	(%res-ndestroy p)))))


;; int gethostname(char *name, size_t len);
(defcfun (%get-host-name "gethostname") :int32
  (name :pointer)
  (len size-t))

;; int getdomainname(char *name, size_t len);
(defcfun (%get-domain-name "getdomainname") :int32
  (name :pointer)
  (len size-t))

(defun get-host-name ()
  (with-foreign-object (p :uint8 256)
    (%get-host-name p 256)
    (let ((hn (foreign-string-to-lisp p)))
      (%get-domain-name p 256)
      (let ((dn (foreign-string-to-lisp p)))
	(values hn dn)))))


;; (defcfun (%eventfd "eventfd") :int32
;;   (initval :int32)
;;   (flags :int32))

;; (defun eventfd (&optional (nonblock-p t) initval)
;;   (%eventfd (or initval 0)
;; 	    (let ((f 0))
;; 	      (when nonblock-p (setf f (logior f 4)))
;; 	      f)))


    
