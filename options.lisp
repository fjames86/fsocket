

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



(defconstant +sol-socket+ #xffff)

;; #define SO_DEBUG        0x0001          /* turn on debugging info recording */
;; #define SO_ACCEPTCONN   0x0002          /* socket has had listen() */
;; #define SO_REUSEADDR    0x0004          /* allow local address reuse */
;; #define SO_KEEPALIVE    0x0008          /* keep connections alive */
;; #define SO_DONTROUTE    0x0010          /* just use interface addresses */
;; #define SO_BROADCAST    0x0020          /* permit sending of broadcast msgs */
;; #define SO_USELOOPBACK  0x0040          /* bypass hardware when possible */
;; #define SO_LINGER       0x0080          /* linger on close if data present */
;; #define SO_OOBINLINE    0x0100          /* leave received OOB data in line */
;; #define SO_DONTLINGER   (int)(~SO_LINGER)
;; #define SO_EXCLUSIVEADDRUSE ((int)(~SO_REUSEADDR)) /* disallow local address reuse */
;; #define SO_SNDBUF       0x1001          /* send buffer size */
;; #define SO_RCVBUF       0x1002          /* receive buffer size */
;; #define SO_SNDTIMEO     0x1005          /* send timeout */
;; #define SO_RCVTIMEO     0x1006          /* receive timeout */
;; #define SO_ERROR        0x1007          /* get error status and clear */
;; #define SO_TYPE         0x1008          /* get socket type */
;; #define SO_REUSEPORT    0x0200          /* allow local address & port reuse */
;; #define SO_NO_OFFLOAD   0x4000          /* socket cannot be offloaded */
;;(defconstant +so-debug+ #x0001)
(defconstant +so-acceptconn+ #x0002)
(defconstant +so-reuseaddr+ #x0004)
;;(defconstant +so-keepalive+ #x0008)
;;(defconstant +so-dontroute+ #x0010)
(defconstant +so-broadcast+ #x00020)
;;(defconstant +so-useloopback+ #x0040)
;;(defconstant +so-linger+ #x0080)
;;(defconstant +so-oobinline+ #x0100)
(defconstant +so-sndbuf+ #x1001)
(defconstant +so-rcvbuf+ #x1002)
(defconstant +so-sndtimeo+ #x1005)
(defconstant +so-rcvtimeo+ #x1006)
;;(defconstant +so-error+ #x1007)
;;(defconstant +so-type+ #x1008)
;;(defconstant +so-resuseport+ #x0200)
;;(defconstant +so-no-offload+ #x4000)

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

;; ------------------------------------------------
;; setting oscket options

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
  (set-socket-option-int32 sock +sol-socket+ +so-rcvtimeo+ value))



#+(or win32 windows)(defconstant +mcast-join-group+ 41) 
#-(or win32 windows)(defconstant +mcast-join-group+ 42) 

#+(or win32 windows)(defconstant +ip-multicast-loop+ 11)
#-(or win32 windows)(defconstant +ip-multicast-loop+ 34)
#+(or win32 windows)(defconstant +ip6-multicast-loop+ 11)
#-(or win32 windows)(defconstant +ip6-multicast-loop+ 19)

#+(or win32 windows)(defconstant +ip-multicast-if+ 11)
#-(or win32 windows)(defconstant +ip-multicast-if+ 32)
#+(or win32 windows)(defconstant +ip6-multicast-if+ 9)
#-(or win32 windows)(defconstant +ip6-multicast-if+ 17)

#+(or win32 windows)(defconstant +ip-multicast-ttl+ 10)
#-(or win32 windows)(defconstant +ip-multicast-ttl+ 33)

;; struct group_req {
;;     ULONG gr_interface;         // Interface index.
;;     SOCKADDR_STORAGE gr_group;  // Multicast address.
;; }
(defcstruct group-req
  (iface :uint32)
  (addr :uint8 :count 32))

(defmethod (setf socket-option) (value sock (level (eql :ip)) (option (eql :mcast-join-group)))
  "VALUE ::= (index address) where INDEX is the interface index and ADDRESS is the multicast address to join."
  (destructuring-bind (index addr) value
    (with-foreign-object (req '(:struct group-req))
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

(defmethod (setf socket-option) (value sock (level (eql :ip)) (option (eql :ip-multicast-if)))
  "Set the local interface to use. VALUE is a 4-octet array indicating the local address."

  ;; we convert the address to 32-bit integer in network-byte-order 
  (let ((v (logior (ash (aref value 0) 24)
                   (ash (aref value 1) 16)
                   (ash (aref value 2) 8)
                   (aref value 3))))
    (set-socket-option-int32 sock +ipproto-ip+ +ip-multicast-if+ v)))
  
(defmethod (setf socket-option) (value sock (level (eql :ip)) (option (eql :ip-multicast-loop)))
  "Set to use or not use loopback. Value is a boolean"
  (set-socket-option-boolean sock +ipproto-ip+ +ip-multicast-loop+ value))


