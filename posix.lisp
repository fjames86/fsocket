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

;; --------------------------------------

(defconstant +socket-error+ -1)
(defun invalid-socket-p (sock)
  (= sock -1))

;; int socket(int socket_family, int socket_type, int protocol);
(defcfun (%socket "socket") :int32
  (family :int32)
  (type :int32)
  (prot :int32))

(defun open-socket (&key (family :inet) (type :datagram) protocol)
    "Open a socket. Call CLOSE-SOCKET to free resources.
FAMILY ::= address family integer. Either :INET or :INET6.
TYPE ::= socket type name, defaults to SOCK_DGRAM. Can be :datagram or :stream.
PROTOCOL ::= socket protocol integer. Usually doesn't need to be specified.

Returns the socket file descriptor."
    (declare (type symbol family type)
             (type (or null integer) protocol))    
    (let ((fd (%socket (ecase family
                         (:inet +af-inet+)
                         (:inet6 +af-inet6+))
                     (ecase type
                       (:stream 1)
                       (:datagram 2))
                     (or protocol 0))))
    (if (invalid-socket-p fd)
        (get-last-error)
        fd)))

;; int close(int fd);
(defcfun (%close "close") :int
  (fd :int32))

(defun close-socket (fd)
  "Close the socket named by FD."
  (let ((sts (%close fd)))
    (if (= sts -1)
        (get-last-error)
        nil)))

;; int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
(defcfun (%bind "bind") :int32
  (fd :int32)
  (addr :pointer)
  (len :int32))

(defun socket-bind (fd addr)
  "Bind the socket to the local address.
FD ::= socket file descriptor.
ADDR ::= local address. Can be either SOCKADDR-IN or SOCKADDR-IN6."
  (etypecase addr
    (sockaddr-in
     (with-foreign-object (p '(:struct sockaddr-in))
       (setf (mem-aref p '(:struct sockaddr-in)) addr)
       (let ((sts (%bind fd p (foreign-type-size '(:struct sockaddr-in)))))
         (if (= sts +socket-error+)
             (get-last-error)
             nil))))
    (sockaddr-in6
     (with-foreign-object (p '(:struct sockaddr-in6))
       (setf (mem-aref p '(:struct sockaddr-in6)) addr)
       (let ((sts (%bind fd p (foreign-type-size '(:struct sockaddr-in6)))))
         (if (= sts +socket-error+)
             (get-last-error)
             nil))))))

;; int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
(defcfun (%connect "connect") :int32
  (fd :int32)
  (addr :pointer)
  (len :int32))

#+linux(defconstant +ewouldblock+ 11)
#+(or freebsd darwin)(defconstant +ewouldblock+ 35)

(defun socket-connect (fd addr)
    "Connect the socket to the remote address.
SOCK :: socket.
ADDR ::= remote address.

Returns true if the operation completed successfully.
If the socket is in non-blocking mode and the operation would block returns NIL. 
A :POLLOUT event indicates a subsequent socket-connect will complete immediately."
    (etypecase addr
      (sockaddr-in
       (with-foreign-object (a '(:struct sockaddr-in))
         (setf (mem-aref a '(:struct sockaddr-in)) addr)
         (let ((sts (%connect fd a (foreign-type-size '(:struct sockaddr-in)))))
           (if (= sts +socket-error+)
               (let ((ecode *errno*))
                 (if (= ecode +ewouldblock+)
                     nil
                     (get-last-error ecode)))
               t))))
      (sockaddr-in6
       (with-foreign-object (a '(:struct sockaddr-in6))
         (setf (mem-aref a '(:struct sockaddr-in6)) addr)
         (let ((sts (%connect fd a (foreign-type-size '(:struct sockaddr-in6)))))
           (if (= sts +socket-error+)
               (let ((ecode *errno*))
                 (if (= ecode +ewouldblock+)
                     nil
                     (get-last-error ecode))
                 t)))))))

;; int listen(int sockfd, int backlog);
(defcfun (%listen "listen") :int32
  (fd :int32)
  (backlog :int32))

(defun socket-listen (fd &optional backlog)
  "Start the socket listening."
  (let ((sts (%listen fd (or backlog 128)))) ;; SOMAXCONN
    (if (= sts +socket-error+)
        (get-last-error)
        nil)))
                      
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
  (with-foreign-objects ((buffer :uint8 32)
                         (alen :uint32))
    (setf (mem-aref alen :uint32) 32)
    (let ((sts (%accept fd
                        buffer
                        alen)))
      (cond
        ((invalid-socket-p sts)
         (let ((ecode *errno*))
           (if (= ecode +ewouldblock+)
               nil
               (get-last-error ecode))))
        (t
         (let ((family #+(or freebsd darwin)(mem-aref buffer :uint8 1)
                       #-(or freebsd darwin)(mem-aref buffer :uint16)))
           (cond 
             ((= family +af-inet+)
              (let ((addr (mem-aref buffer '(:struct sockaddr-in))))
                (values sts addr)))
             ((= family +af-inet6+)
              (let ((addr (mem-aref buffer '(:struct sockaddr-in6))))
                (values sts addr)))
             (t
              (close-socket sts)
              (error 'fsocket-error :msg (format nil "Unknown address family ~A" family))))))))))

;; int shutdown(int sockfd, int how);
(defcfun (%shutdown-socket "shutdown") :int32
  (fd :int32)
  (how :int32))

(defun socket-shutdown (fd &optional (how :receive))
  "Shutdown traffic on the TCP socket.
HOW ::= :SEND to stop sending, :RECEIVE to stop receiving, :BOTH to stop both."
  (let ((sts
         (%shutdown-socket fd
                           (ecase how
                             (:send 1)
                             (:both 2)
                             (:receive 0)))))
    (if (= sts +socket-error+)
        (get-last-error)
        nil)))

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

(declaim (ftype (function (t (vector (unsigned-byte 8)) &key (:start integer) (:end (or null integer)))
			  integer)))
(defun socket-send (fd buffer &key (start 0) end)
  "Send a buffer on the connected socket.
SOCK ::= connected socket.
BUFFER ::= octet array.
START ::= start index of buffer.
END ::= end index of buffer.

Returns the number of bytes actually sent, which can be less than the requested length."
  (let ((count (- (or end (length buffer)) start)))
    (with-foreign-object (p :uint8 count)
      (dotimes (i count)
        (setf (mem-aref p :uint8 i) (aref buffer (+ start i))))
      (let ((sts (%send fd p count 0)))
        (if (= sts +socket-error+)
            (get-last-error)
            sts)))))

;; ssize_t sendto(int sockfd, const void *buf, size_t len, int flags,
;;                const struct sockaddr *dest_addr, socklen_t addrlen);
(defcfun (%sendto "sendto") ssize-t
  (fd :int32)
  (buffer :pointer)
  (len size-t)
  (flags :int32)
  (addr :pointer)
  (alen :int32))

(declaim (ftype (function (t (vector (unsigned-byte 8)) (or sockaddr-in sockaddr-in6) &key (:start integer) (:end (or null integer)))
			  integer)))
(defun socket-sendto (fd buffer addr &key (start 0) end)
  "Send data to the address on the socket.
SOCK ::= socket.
BUFFER ::= octet array
ADDR ::= destination address, either a SOCKADDR-IN or SOCKADDR-IN6 structure.
START ::= buffer start index
END ::= buffer end index.

Returns the number of octets actually sent, which can be less than the number requested."
  (let ((count (- (or end (length buffer)) start))
        (alen 0))
    (with-foreign-objects ((p :uint8 count)
                           (a :uint8 32))
      (dotimes (i count)
        (setf (mem-aref p :uint8 i)
              (aref buffer (+ start i))))
      (etypecase addr
        (sockaddr-in
         (setf (mem-aref a '(:struct sockaddr-in)) addr
               alen (foreign-type-size '(:struct sockaddr-in))))
        (sockaddr-in6
         (setf (mem-aref a '(:struct sockaddr-in6)) addr
               alen (foreign-type-size '(:struct sockaddr-in6)))))
      (let ((sts (%sendto fd
                          p
                          count
                          0
                          a
                          alen)))
        (if (= sts +socket-error+)
            (get-last-error)
            sts)))))


;; ssize_t recv(int sockfd, void *buf, size_t len, int flags);
(defcfun (%recv "recv") ssize-t
  (fd :int32)
  (buffer :pointer)
  (len size-t)
  (flags :int32))

(declaim (ftype (function (t (vector (unsigned-byte 8)) &key (:start integer) (:end (or null integer)))
			  integer)))
(defun socket-recv (fd buffer &key (start 0) end)
  "Receive data from the socket.
SOCK ::= socket.
BUFFER ::= octet array to receive data into.
START ::= buffer start idnex.
END ::= buffer end index.

Retuns the number of bytes actually received, which can be less than the number requested."
  (let ((count (- (or end (length buffer)) start)))
    (with-foreign-object (p :uint8 count)
      (let ((sts (%recv fd p count 0)))
        (cond
          ((= sts +socket-error+)
           (get-last-error))
          (t
           (dotimes (i sts)
             (setf (aref buffer (+ start i))
                   (mem-aref p :uint8 i)))
           sts))))))

;;ssize_t recvfrom(int sockfd, void *buf, size_t len, int flags,
;;                 struct sockaddr *src_addr, socklen_t *addrlen);
(defcfun (%recvfrom "recvfrom") ssize-t
  (fd :int32)
  (buffer :pointer)
  (len size-t)
  (flags :int32)
  (addr :pointer)
  (alen :pointer))

(declaim (ftype (function (t (vector (unsigned-byte 8)) &key (:start integer) (:end (or null integer)))
			  (values integer (or sockaddr-in sockaddr-in6)))))
(defun socket-recvfrom (fd buffer &key (start 0) end)
  "Receive data from the socket.
SOCK ::= socket.
BUFFER ::= octet array
START ::= start index
END ::= end index.

Returns (values count addr) where
COUNT ::= number of octets actually received, which can be less tha nthe number requested.
ADDR ::= remote address from which the data was received.
"
  (let ((count (- (or end (length buffer)) start)))
    (with-foreign-objects ((p :uint8 count)
                           (a :uint8 32)
                           (alen :uint32))
      (setf (mem-aref alen :uint32) 32)
      (let ((sts (%recvfrom fd
                            p
                            count
                            0
                            a
                            alen)))
        (cond
          ((= sts +socket-error+)
           (get-last-error))
          (t
           (dotimes (i sts)
             (setf (aref buffer (+ start i)) (mem-ref p :uint8 i)))
           (let ((family #+(or freebsd darwin)(mem-aref a :uint8 1)
                         #-(or freebsd darwin)(mem-aref a :uint16)))
             (cond
               ((= family +af-inet+)
                (let ((addr (mem-aref a '(:struct sockaddr-in))))
                  (values sts addr)))
               ((= family +af-inet6+)
                (let ((addr (mem-aref a '(:struct sockaddr-in6))))
                  (values sts addr)))
               (t (error 'fsocket-error :msg (format nil "Unknown address family ~A" family)))))))))))


;; int getsockname(int socket, struct sockaddr *restrict address, socklen_t *restrict address_len);
(defcfun (%getsockname "getsockname") :int32
  (fd :int32)
  (addr :pointer)
  (len :pointer))
(defun socket-name (fd)
    "Get the address to which the socket has been bound.
FD ::= socket as returned from OPEN-SOCKET.
Returns a SOCKADDR-IN or SOCKADDR-IN6 structure."
  (with-foreign-objects ((addr :uint32 32)
                         (len :uint32))
    (setf (mem-aref len :uint32) 32)
    (let ((sts (%getsockname fd addr len)))
      (when (= sts +socket-error+)
        (get-last-error))
      (let ((family #+(or freebsd darwin)(mem-aref addr :uint8 1)
                    #-(or freebsd darwin)(mem-aref addr :uint16)))
        (cond
          ((= family +af-inet+)
           (mem-aref addr '(:struct sockaddr-in)))
          ((= family +af-inet6+)
           (mem-aref addr '(:struct sockaddr-in6)))
          (t (error 'fsocket-error :msg (format nil "Unknown address family ~A" family))))))))




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

#+linux(defconstant +fionbio+ 0x5421)
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
(defcunion ifreq-data
  (addr (:struct sockaddr-in))
  (flags :uint16)
  (ivalue :int32)
  (mtu :int32))

(defcstruct ifreq
  (name :uint8 :count 16)
  (data (:union ifreq-data)))

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

;;#+(or win32 windows)(defconstant +siocgifmtu+ #x8921)
#+linux(defconstant +siocgifmtu+ #x8921)
#+freebsd(defconstant +siocgifmtu+ #x8921)
#+darwin(defconstant +siocgifmtu+ #xC0206933)

#+linux(defconstant +siocgifhwaddr+ #x8927)
#+freebsd(defconstant +af-link+ #x12)
#+darwin(defconstant +af-link+ #x12)

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
  (options :uint64)
  (nscount :int32)
  (nsaddrs (:struct sockaddr-in) :count 3)) ;; MAXNS == 3


;; (define-foreign-library lresolv
;;   (t (:default "libresolv")))

;; (use-foreign-library lresolv)

(defcfun (%res-ninit "__res_ninit") :int32
  (state :pointer))

(defun get-name-servers ()
  "Returns a list of SOCKADDR-IN addresses for configured DNS name servers."
  (with-foreign-object (p :uint8 600) ;; allocate more than we need just in case
    ;; clear it out
    (dotimes (i 600)
      (setf (mem-aref p :uint8 i) 0))
    
    (let ((sts (%res-ninit p)))
      (unless (zerop sts) (get-last-error))
      (let ((ap (foreign-slot-pointer p '(:struct res-state) 'nsaddrs)))
        (loop :for i :below (foreign-slot-value p '(:struct res-state) 'nscount)
           :collect (mem-aref ap '(:struct sockaddr-in) i))))))


