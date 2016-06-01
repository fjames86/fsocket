;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file defines all the functions for Windows

(in-package #:fsocket)

;; For when we need to allocate some memory to hold addresses 
(defconstant +sockaddr-storage+ 32)

;; for windows errors
(defcfun (%format-message "FormatMessageA" :convention :stdcall)
    :uint32
  (flags :uint32)
  (source :pointer)
  (msg-id :uint32)
  (lang-id :uint32)
  (buffer :pointer)
  (size :uint32)
  (args :pointer))

(defun format-message (code)
  "Use FormatMessage to convert the error code into a system-defined string."
  (with-foreign-object (buffer :char 1024)
    (let ((n (%format-message #x00001000
                              (null-pointer)
                              code
                              0
                              buffer
                              1024
                              (null-pointer))))
      (if (= n 0)
          (error 'fsocket-error :msg "Failed to format message")
          (foreign-string-to-lisp buffer :count (- n 2))))))

(define-condition win-error (fsocket-error)
  ((code :initform 0 :initarg :code :reader win-error-code))
  (:report (lambda (condition stream)
             (format stream "FSOCKET-ERROR ~A (0x~X): ~A" 
                     (win-error-code condition)
                     (win-error-code condition)
                     (format-message (win-error-code condition))))))

;; GetLastError or WSAGetLastError?
(defcfun (%get-last-error "GetLastError" :convention :stdcall) :long)

(defun get-last-error (&optional ecode)
  (let ((code (or ecode (%get-last-error))))
    (error 'win-error :code code)))


(defconstant +wsa-error-msgsize+ #x2738)
(defconstant +wsa-error-wouldblock+ 10035)

;; ------------------ startup -------------------------

;; typedef struct WSAData {
;;         WORD                    wVersion;
;;         WORD                    wHighVersion;
;; #ifdef _WIN64
;;         unsigned short          iMaxSockets;
;;         unsigned short          iMaxUdpDg;
;;         char FAR *              lpVendorInfo;
;;         char                    szDescription[WSADESCRIPTION_LEN+1];
;;         char                    szSystemStatus[WSASYS_STATUS_LEN+1];
;; #else
;;         char                    szDescription[WSADESCRIPTION_LEN+1];
;;         char                    szSystemStatus[WSASYS_STATUS_LEN+1];
;;         unsigned short          iMaxSockets;
;;         unsigned short          iMaxUdpDg;
;;         char FAR *              lpVendorInfo;
;; #endif
;; } WSADATA, FAR * LPWSADATA;
(defcstruct wsadata
  (version :uint16)
  (highversion :uint16)
  #+(or x64 x86-64 amd64)(max-sock :uint16)
  #+(or x64 x86-64 amd64)(max-datagram :uint16)
  #+(or x64 x86-64 amd64)(vendor-info :pointer)
  #+(or x64 x86-64 amd64)(desc :char :count 257)
  #+(or x64 x86-64 amd64)(status :char :count 129)
  #-(or x64 x86-64 amd64)(desc :char :count 257)
  #-(or x64 x86-64 amd64)(status :char :count 129)
  #-(or x64 x86-64 amd64)(max-sock :uint16)
  #-(or x64 x86-64 amd64)(max-datagram :uint16)
  #-(or x64 x86-64 amd64)(vendor-info :pointer))
  
  
;; int WSAStartup(
;;   _In_  WORD      wVersionRequested,
;;   _Out_ LPWSADATA lpWSAData
;; );
(defcfun (%wsa-startup "WSAStartup") :int32
  (version :uint16)
  (data :pointer))
(defun wsa-startup ()
  (with-foreign-object (wsadata '(:struct wsadata))
    ;; MAKEWORD(2,2) == #x0202
    (%wsa-startup #x0202 wsadata)))

;; int WSACleanup(void);
(defcfun (wsa-cleanup "WSACleanup") :int)

(let ((initialized nil))
  (defun winsock-init ()
    (unless initialized 
      (wsa-startup)
      (setf initialized t))))

(winsock-init)

;; ----------------------------------------------------

;; BOOL WINAPI CloseHandle(
;;   _In_ HANDLE hObject
;; );
(defcfun (close-handle "CloseHandle" :convention :stdcall) :boolean
  (handle :pointer))

;; SOCKET WSAAPI socket(
;;   _In_ int af,
;;   _In_ int type,
;;   _In_ int protocol
;; );
(defcfun (%socket "socket" :convention :stdcall)
    :pointer
  (family :int32)
  (type :int32)
  (prot :int32))


(defun invalid-socket-p (sock)
  (= (pointer-address sock)
     #+(or amd64 x86-64 x64)#xffffffffffffffff
     #-(or amd64 x86-64 x64)#xffffffff))

(defconstant +socket-error+ -1)

(defun open-socket (&key (family :inet) (type :datagram) protocol)
  "Open a socket. Call CLOSE-SOCKET to free resources.
FAMILY ::= address family integer. Either :INET or :INET6.
TYPE ::= socket type name, defaults to SOCK_DGRAM. Can be :datagram or :stream.
PROTOCOL ::= socket protocol integer. 

Returns the socket or errors on failure."
  (declare (type symbol family type)
           (type (or null integer) protocol))
  (let ((s (%socket (ecase family
                      (:inet +af-inet+)
                      (:inet6 +af-inet6+))
                    (ecase type
                      (:datagram +sock-dgram+)
                      (:stream +sock-stream+))
                    (or protocol 0))))
    (if (invalid-socket-p s)
        (get-last-error)
        s)))

;; int closesocket(
;;   _In_ SOCKET s
;; );
(defcfun (%close-socket "closesocket") :int32
  (sock :pointer))

(defun close-socket (sock)
  "Close the socket."
  (let ((sts (%close-socket sock)))
    (if (= sts +socket-error+)
        (get-last-error)
        nil)))

;; int bind(
;;   _In_ SOCKET                s,
;;   _In_ const struct sockaddr *name,
;;   _In_ int                   namelen
;; );l
(defcfun (%bind "bind") :int32
  (sock :pointer)
  (addr :pointer)
  (len :int32))

(defun socket-bind (sock addr)
  "Bind the socket to the local address.
SOCK ::= socket.
ADDR ::= local address. SOCKADDR-IN or SOCKADDR-IN6 address."
  (with-foreign-object (a :uint8 +sockaddr-storage+)
    (let ((len +sockaddr-storage+))
      (etypecase addr
	(sockaddr-in
	 (setf (mem-aref a '(:struct sockaddr-in)) addr
	       len (foreign-type-size '(:struct sockaddr-in))))
	(sockaddr-in6
	 (setf (mem-aref a '(:struct sockaddr-in6)) addr
	       len (foreign-type-size '(:struct sockaddr-in6)))))
      (let ((sts (%bind sock a len)))
	(if (= sts +socket-error+)
	    (get-last-error)
	    nil)))))

;; int connect(
;;   _In_ SOCKET                s,
;;   _In_ const struct sockaddr *name,
;;   _In_ int                   namelen
;; );
(defcfun (%connect "connect") :int32
  (sock :pointer)
  (addr :pointer)
  (len :int32))

(defun socket-connect (sock addr)
  "Connect the socket to the remote address.
SOCK :: socket.
ADDR ::= remote address.

Returns true if the connection was established or nil if a non-blocking
connect was initiated. In this case you should wait using POLL for 
a :POLLOUT status and call SOCKET-CONNECT again. 
If the connection fails an error is signalled."
  (with-foreign-object (a :uint8 +sockaddr-storage+)
    (let ((len +sockaddr-storage+))
      (etypecase addr
	(sockaddr-in	 
	 (setf (mem-aref a '(:struct sockaddr-in)) addr
	       len (foreign-type-size '(:struct sockaddr-in))))
	(sockaddr-in6
	 (setf (mem-aref a '(:struct sockaddr-in6)) addr
	       len (foreign-type-size '(:struct sockaddr-in6)))))
       (let ((sts (%connect sock a len)))
         (if (= sts +socket-error+)
	     (let ((ecode (%get-last-error)))
	       (if (= ecode +wsa-error-wouldblock+)
		   nil
		   (get-last-error ecode)))
	     t)))))

;; int listen(
;;   _In_ SOCKET s,
;;   _In_ int    backlog
;; );
(defcfun (%listen "listen") :int32
  (sock :pointer)
  (backlog :int32))

(defun socket-listen (sock &optional backlog)
  "Start the socket listening."
  (let ((sts (%listen sock (or backlog #x7fffffff)))) ;; SOMAXCONN
    (if (= sts +socket-error+)
        (get-last-error)
        nil)))

;; SOCKET accept(
;;   _In_    SOCKET          s,
;;   _Out_   struct sockaddr *addr,
;;   _Inout_ int             *addrlen
;; );
(defcfun (%accept "accept") :pointer
  (sock :pointer)
  (addr :pointer)
  (alen :pointer))

(defun socket-accept (sock)
  "Accept a connection from the listening socket.
SOCK ::= listening socket.

Returns (values conn addr) where
CONN ::= new connected socket.
ADDR ::= address of the connected socket.

Returns the connection socket descriptor if completed or nil if 
the socket is in non-blocking mode and the operation would block."
  (with-foreign-objects ((buffer :uint8 +sockaddr-storage+)
                         (alen :uint32))
    (setf (mem-aref alen :uint32) +sockaddr-storage+)
    (let ((sts (%accept sock buffer alen)))
      (cond
        ((invalid-socket-p sts)
         (let ((ecode (%get-last-error)))
           (if (= ecode +wsa-error-wouldblock+)
               nil
               (get-last-error ecode))))
        (t
	 (handler-bind ((error (lambda (e)
				 (declare (ignore e))
				 (close-socket sts)
				 nil)))
	   (values sts (translate-sockaddr-from-foreign buffer))))))))
                                 
;; int shutdown(
;;   _In_ SOCKET s,
;;   _In_ int    how
;; );
(defcfun (%shutdown-socket "shutdown") :int32
  (sock :pointer)
  (how :int32))

(defun socket-shutdown (sock &optional (how :receive))
  "Shutdown traffic on the TCP socket.
HOW ::= :SEND to stop sending, :RECEIVE to stop receiving, :BOTH to stop both."
  (let ((sts
         (%shutdown-socket sock
                           (ecase how
                             (:send 1)
                             (:both 2)
                             (:receive 0)))))
    (if (= sts +socket-error+)
        (get-last-error)
        nil)))


;; -----------------------------------------------------

;; int send(
;;   _In_       SOCKET s,
;;   _In_ const char   *buf,
;;   _In_       int    len,
;;   _In_       int    flags
;; );
(defcfun (%send "send") :int32
  (sock :pointer)
  (buffer :pointer)
  (len :int32)
  (flags :int32))

(declaim (ftype (function (t (vector (unsigned-byte 8)) &key (:start integer) (:end (or null integer)))
			  integer)))
(defun socket-send (sock buffer &key (start 0) end)
  "Send a buffer on the connected socket.
SOCK ::= connected socket.
BUFFER ::= octet array.
START ::= start index of buffer.
END ::= end index of buffer.

Returns the number of bytes actually sent, which can be less than the requested length."
  (declare (type (vector (unsigned-byte 8)) buffer))
  (let ((count (- (or end (length buffer)) start)))
    (with-foreign-object (p :uint8 count)
      (dotimes (i count)
        (setf (mem-aref p :uint8 i)
              (aref buffer (+ start i))))
      (let ((sts (%send sock p count 0)))
        (if (= sts +socket-error+)
            (get-last-error)
            sts)))))

;; int sendto(
;;   _In_       SOCKET                s,
;;   _In_ const char                  *buf,
;;   _In_       int                   len,
;;   _In_       int                   flags,
;;   _In_       const struct sockaddr *to,
;;   _In_       int                   tolen
;; );
(defcfun (%sendto "sendto") :int32
  (sock :pointer)
  (buffer :pointer)
  (len :int32)
  (flags :int32)
  (addr :pointer)
  (alen :int32))

(declaim (ftype (function (t (vector (unsigned-byte 8)) (or sockaddr-in sockaddr-in6) &key (:start integer) (:end (or null integer)))
			  integer)))
(defun socket-sendto (sock buffer addr &key (start 0) end)
  "Send data to the address on the socket.
SOCK ::= socket.
BUFFER ::= octet array
ADDR ::= destination address, either a SOCKADDR-IN or SOCKADDR-IN6 structure.
START ::= buffer start index
END ::= buffer end index.

Returns the number of octets actually sent, which can be less than the number requested."
  (declare (type (vector (unsigned-byte 8)) buffer)
	   (type (or sockaddr-in sockaddr-in6) addr))
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
               alen (foreign-type-size '(:struct sockaddr-in6)))))
      (let ((sts (%sendto sock
                          p
                          count
                          0
                          a
                          alen)))
        (if (= sts +socket-error+)
            (get-last-error)
            sts)))))

;; int recv(
;;   _In_  SOCKET s,
;;   _Out_ char   *buf,
;;   _In_  int    len,
;;   _In_  int    flags
;; );
(defcfun (%recv "recv") :int32
  (sock :pointer)
  (buffer :pointer)
  (len :int32)
  (flags :int32))

(declaim (ftype (function (t (vector (unsigned-byte 8)) &key (:start integer) (:end (or null integer)))
			  integer)))
(defun socket-recv (sock buffer &key (start 0) end)
  "Receive data from the socket.
SOCK ::= socket.
BUFFER ::= octet array to receive data into.
START ::= buffer start idnex.
END ::= buffer end index.

Retuns the number of bytes actually received, which can be less than the number requested."
  (declare (type (vector (unsigned-byte 8)) buffer))
  (let ((count (- (or end (length buffer)) start)))
    (with-foreign-object (p :uint8 count)
      (let ((sts (%recv sock p count 0)))
        (cond
          ((= sts +socket-error+) (get-last-error))
          (t
           (dotimes (i sts)
             (setf (aref buffer (+ start i))
                   (mem-aref p :uint8 i)))
           sts))))))
            
;; int recvfrom(
;;   _In_        SOCKET          s,
;;   _Out_       char            *buf,
;;   _In_        int             len,
;;   _In_        int             flags,
;;   _Out_       struct sockaddr *from,
;;   _Inout_opt_ int             *fromlen
;; );
(defcfun (%recvfrom "recvfrom") :int32
  (sock :pointer)
  (buffer :pointer)
  (len :int32)
  (flags :int32)
  (addr :pointer)
  (alen :pointer))

(declaim (ftype (function (t (vector (unsigned-byte 8)) &key (:start integer) (:end (or null integer)))
			  (values integer (or sockaddr-in sockaddr-in6)))))
(defun socket-recvfrom (sock buffer &key (start 0) end)
  "Receive data from the socket.
SOCK ::= socket.
BUFFER ::= octet array
START ::= start index into BUFFER where received data will be placed.
END ::= end index into BUFFER where received data will be placed.

Returns (values count addr) where
COUNT ::= number of octets actually received, which can be less than the number requested.
ADDR ::= remote address from which the data was received.
"
  (declare (type (vector (unsigned-byte 8)) buffer))
  (let ((count (- (or end (length buffer)) start)))
    (with-foreign-objects ((p :uint8 count)
                           (a :uint8 +sockaddr-storage+)
                           (alen :uint32))
      (setf (mem-aref alen :uint32) +sockaddr-storage+)
      (let ((sts (%recvfrom sock
                            p
                            count
                            0
                            a
                            alen)))
        (cond
          ((= sts +socket-error+) (get-last-error))
          (t
           (dotimes (i sts)
             (setf (aref buffer (+ start i)) (mem-ref p :uint8 i)))
	   (values sts (translate-sockaddr-from-foreign a))))))))

;; int getsockname(
;;   _In_    SOCKET          s,
;;   _Out_   struct sockaddr *name,
;;   _Inout_ int             *namelen
;; );
(defcfun (%getsockname "getsockname") :int32
  (sock :pointer)
  (addr :pointer)
  (len :pointer))

(defun socket-name (sock)
  "Get the address to which the socket has been bound.
SOCK ::= socket as returned from OPEN-SOCKET.
Returns a SOCKADDR-IN or SOCKADDR-IN6 structure."
  (with-foreign-objects ((addr :uint32 32)
                         (len :uint32))
    (setf (mem-aref len :uint32) 32)
    (let ((sts (%getsockname sock addr len)))
      (if (= sts +socket-error+)
	  (get-last-error)
	  (translate-sockaddr-from-foreign addr)))))
          
;; -----------------------------------------------------

;; int getsockopt(
;;   _In_    SOCKET s,
;;   _In_    int    level,
;;   _In_    int    optname,
;;   _Out_   char   *optval,
;;   _Inout_ int    *optlen
;; );
(defcfun (%getsockopt "getsockopt") :int32
  (sock :pointer)
  (level :int32)
  (option :int32)
  (val :pointer)
  (vlen :pointer))

;; int setsockopt(
;;   _In_       SOCKET s,
;;   _In_       int    level,
;;   _In_       int    optname,
;;   _In_ const char   *optval,
;;   _In_       int    optlen
;; );
(defcfun (%setsockopt "setsockopt") :int32
  (sock :pointer)
  (level :int32)
  (option :int32)
  (val :pointer)
  (len :int32))
     
;; ------------------------------------------------------

;; WSAEVENT WSACreateEvent(void);
(defcfun (%wsa-create-event "WSACreateEvent") :pointer)

;; DWORD WSAWaitForMultipleEvents(
;;   _In_       DWORD    cEvents,
;;   _In_ const WSAEVENT *lphEvents,
;;   _In_       BOOL     fWaitAll,
;;   _In_       DWORD    dwTimeout,
;;   _In_       BOOL     fAlertable
;; );
(defcfun (%wsa-wait-for-multiple-events "WSAWaitForMultipleEvents" :convention :stdcall)
    :uint32
  (count :uint32)
  (events :pointer)
  (waitall :boolean)
  (timeout :uint32)
  (alertable :boolean))
(defun wsa-wait-for-multiple-events (events &key (timeout 0) waitall alertable)
  (let ((count (length events)))
    (with-foreign-object (p :pointer count)
      (do ((%events events (cdr %events))
           (i 0 (1+ i)))
          ((null %events))
        (setf (mem-aref p :pointer i) (car %events)))
      (%wsa-wait-for-multiple-events count p waitall timeout alertable))))
(defun wsa-wait-for-single-event (event &key (timeout 0) waitall alertable)
  (with-foreign-object (p :pointer)
    (setf (mem-aref p :pointer) event)
    (%wsa-wait-for-multiple-events 1 p waitall timeout alertable)))

;; BOOL WSACloseEvent(
;;   _In_ WSAEVENT hEvent
;; );
(defcfun (%wsa-close-event "WSACloseEvent") :boolean
  (event :pointer))
(defun wsa-close-event (event)
  (let ((sts (%wsa-close-event event)))
    (if sts
        nil
        (get-last-error))))
                       
;; int WSAEventSelect(
;;   _In_ SOCKET   s,
;;   _In_ WSAEVENT hEventObject,
;;   _In_ long     lNetworkEvents
;; );
(defcfun (%wsa-event-select "WSAEventSelect") :int32
  (sock :pointer)
  (event :pointer)
  (events :uint32))

(defun wsa-event-select (sock event &rest events)
  (let ((evt 0))
    (dolist (e events)
      (setf evt
            (logior evt 
                    (ecase e
                      ;; we need to map :pollin event to read and accept to preserve the semantics of posix poll
                      (:pollin (logior #x1 #x8)) ;; FD_READ|FD_ACCEPT
                      (:pollout #x2) ;; FD_WRITE
                      (:pollhup #x20) ;; FD_CLOSE. Windows requires us to explicitly request it, c.f. posix. 
                      ))))
    (let ((sts (%wsa-event-select sock event evt)))
      (if (= sts +socket-error+)
          (get-last-error)
          nil))))


;; int WSAEnumNetworkEvents(
;;   _In_  SOCKET             s,
;;   _In_  WSAEVENT           hEventObject,
;;   _Out_ LPWSANETWORKEVENTS lpNetworkEvents
;; );
(defcfun (%wsa-enum-network-events "WSAEnumNetworkEvents") :int32
  (sock :pointer)
  (event :pointer)
  (events :pointer))

;; typedef struct _WSANETWORKEVENTS {
;;   long lNetworkEvents;
;;   int  iErrorCode[FD_MAX_EVENTS];
;; } WSANETWORKEVENTS, *LPWSANETWORKEVENTS;
(defcstruct wsa-network-events 
  (events :uint32)
  (ierrors :uint32 :count 10))

(defun wsa-enum-network-events (sock event)
  (with-foreign-object (events '(:struct wsa-network-events))
    ;; memset to zero first 
    (dotimes (i (foreign-type-size '(:struct wsa-network-events)))
      (setf (mem-aref events :uint8 i) 0))
    
    ;; enumerate the events 
    (let ((sts (%wsa-enum-network-events sock event events)))
      (if (= sts +socket-error+)
          (get-last-error)
          (do ((ret nil)
               (i 0 (1+ i))
               (ev (foreign-slot-value events '(:struct wsa-network-events) 'events)))
              ((= i 6) (nreverse ret))
            (unless (zerop (logand ev (ash 1 i)))
              (push (ecase i
                      (0 :pollin) ;; FD_READ
                      (1 :pollout) ;; FD_WRITE
                      #+nil(2 :oob) ;; FD_OOB
                      (3 :pollin) ;; FD_ACCEPT. We map this to pollin to preserve semantics with posix 
                      (4 :pollout) ;; FD_CONNECT. We map this to pollout to preserve semantics with posix
                      (5 :pollhup)) ;; FD_CLOSE 
                    ret))
            (let ((ecode (mem-aref (foreign-slot-value events '(:struct wsa-network-events) 'ierrors)
                                   :uint32
                                   i)))
              (unless (zerop ecode)
                (push :pollerr ret))))))))

(defstruct poll-context
  event
  fds)

(defun open-poll ()
  "Open a poll context."
  (let ((event (%wsa-create-event)))
    (when (invalid-socket-p event) (get-last-error))
    (make-poll-context :event event
                       :fds nil)))

(defun close-poll (pc)
  "Close poll context."
  (declare (type poll-context pc))
  (wsa-close-event (poll-context-event pc)))

(declaim (ftype (function (poll-context pollfd) pollfd) poll-register))
(defun poll-register (pc pollfd)
  "Register a pollfd descriptor with the poll context."
  (declare (type poll-context pc)
	   (type pollfd pollfd))

  ;; register with the event
  (apply #'wsa-event-select
         (pollfd-fd pollfd)
         (poll-context-event pc)
         (poll-events (pollfd-events pollfd)))
  
  ;; push onto the fds list 
  (push pollfd (poll-context-fds pc))
  
  pollfd)

(declaim (ftype (function (poll-context pollfd) null)))
(defun poll-unregister (pc pollfd)
  "Unregister a pollfd descriptor from the poll context."
  (declare (type poll-context pc)
	   (type pollfd pollfd))

  ;; all we have to do is remove it from the list of descriptors.
  ;; We assume the user has called close-socket on the fd
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
        :test #'pointer-eq))

;;(defconstant +wsa-wait-event-0+ 0)
(defconstant +wsa-wait-timeout+ 258)

(declaim (ftype (function (poll-context &key (:timeout (or null integer))) list) poll))
(defun poll (pc &key timeout)
  "Poll the sockets registered with the poll context for network events.

PC ::= poll context as returne from OPEN-POLL.
TIMEOUT ::= if supplied time in milliseconds to wait. If not supplied defaults to infinity.

Returns a list of registered pollfd structures. Users should check the REVENTS slot for pending events.
"
  (declare (type poll-context pc)
           (type (or null integer) timeout))

  ;; reset all the events???
  ;; (dolist (pfd (poll-context-fds pc))
  ;;   (apply #'wsa-event-select
  ;;          (pollfd-fd pfd)
  ;;          (poll-context-event pc)
  ;;          (poll-events (pollfd-events pfd))))
  
  ;; start by waiting on the event
  (let ((sts
         (wsa-wait-for-single-event (poll-context-event pc)
                                    :timeout (or timeout #xffffffff)
                                    :waitall t
                                    :alertable nil)))
    (unless (zerop sts)
      (cond
        ((= sts +wsa-wait-timeout+)
         ;; timeout, return NIL
         (return-from poll nil))
        (t
         ;; error
         (get-last-error))))
    ;; now enumerate the events
    (do ((fds (poll-context-fds pc) (cdr fds))
         (ret (poll-context-fds pc)))
        ((null fds) ret)
      (let ((revents (apply #'poll-events
                            ;; wsa-enum-network-events returns a list of pollfd event names, i.e. :pollin etc
                            (wsa-enum-network-events (pollfd-fd (car fds))
                                                     (poll-context-event pc)))))
	(setf (pollfd-revents (car fds)) revents)))))


;; -------------------------------------------------

;; Below we get a list of host adapters. 

;; struct _IP_ADAPTER_UNICAST_ADDRESS_XP {
;;     union {
;;         ULONGLONG Alignment;
;;         struct { 
;;             ULONG Length;
;;             DWORD Flags;
;;         };
;;     };
;;     struct _IP_ADAPTER_UNICAST_ADDRESS_XP *Next;
;;     SOCKET_ADDRESS Address;

;;     IP_PREFIX_ORIGIN PrefixOrigin;
;;     IP_SUFFIX_ORIGIN SuffixOrigin;
;;     IP_DAD_STATE DadState;

;;     ULONG ValidLifetime;
;;     ULONG PreferredLifetime;
;;     ULONG LeaseLifetime;
;; }
(defcstruct ip-adapter-unicast-address
  (length :uint32)
  (flags :uint32)
  (next :pointer)
  (addr :pointer)
  (addr-len :uint32)
  (prefix-origin :uint32)
  (suffix-origin :uint32)
  (dad-state :uint32)
  (valid-lifetime :uint32)
  (preferred-lifetime :uint32)
  (lease-lifetime :uint32))

;; struct _IP_ADAPTER_ANYCAST_ADDRESS_XP {
;;     union {
;;         ULONGLONG Alignment;
;;         struct { 
;;             ULONG Length;
;;             DWORD Flags;
;;         };
;;     };
;;     struct _IP_ADAPTER_ANYCAST_ADDRESS_XP *Next;
;;     SOCKET_ADDRESS Address;
;; } 
(defcstruct ip-adapter-anycast-address
  (length :uint32)
  (flags :uint32)
  (next :pointer)
  (addr :pointer)
  (len :uint32))

;; struct _IP_ADAPTER_MULTICAST_ADDRESS_XP {
;;     union {
;;         ULONGLONG Alignment;
;;         struct {
;;             ULONG Length;
;;             DWORD Flags;
;;         };
;;     };
;;     struct _IP_ADAPTER_MULTICAST_ADDRESS_XP *Next;
;;     SOCKET_ADDRESS Address;
;; } 
(defcstruct ip-adapter-multicast-address
  (length :uint32)
  (flags :uint32)
  (next :pointer)
  (addr :pointer)
  (len :uint32))


;; struct _IP_ADAPTER_DNS_SERVER_ADDRESS_XP {
;;     union {
;;         ULONGLONG Alignment;
;;         struct {
;;             ULONG Length;
;;             DWORD Reserved;
;;         };
;;     };
;;     struct _IP_ADAPTER_DNS_SERVER_ADDRESS_XP *Next;
;;     SOCKET_ADDRESS Address;
;; }
(defcstruct ip-adapter-dns-server-address
  (length :uint32)
  (flags :uint32)
  (next :pointer)
  (addr :pointer)
  (len :uint32))


;; struct _IP_ADAPTER_ADDRESSES_LH {
;;     union {
;;         ULONGLONG Alignment;
;;         struct {
;;             ULONG Length;
;;             IF_INDEX IfIndex;
;;         };
;;     };
;;     struct _IP_ADAPTER_ADDRESSES_LH *Next;
;;     PCHAR AdapterName;
;;     PIP_ADAPTER_UNICAST_ADDRESS_LH FirstUnicastAddress;
;;     PIP_ADAPTER_ANYCAST_ADDRESS_XP FirstAnycastAddress;
;;     PIP_ADAPTER_MULTICAST_ADDRESS_XP FirstMulticastAddress;
;;     PIP_ADAPTER_DNS_SERVER_ADDRESS_XP FirstDnsServerAddress;
;;     PWCHAR DnsSuffix;
;;     PWCHAR Description;
;;     PWCHAR FriendlyName;
;;     BYTE PhysicalAddress[MAX_ADAPTER_ADDRESS_LENGTH];
;;     ULONG PhysicalAddressLength;
;;     union {
;;         ULONG Flags;
;;         struct {
;;             ULONG DdnsEnabled : 1;
;;             ULONG RegisterAdapterSuffix : 1;
;;             ULONG Dhcpv4Enabled : 1;
;;             ULONG ReceiveOnly : 1;
;;             ULONG NoMulticast : 1;
;;             ULONG Ipv6OtherStatefulConfig : 1;
;;             ULONG NetbiosOverTcpipEnabled : 1;
;;             ULONG Ipv4Enabled : 1;
;;             ULONG Ipv6Enabled : 1;
;;             ULONG Ipv6ManagedAddressConfigurationSupported : 1;
;;         };
;;     };
;;     ULONG Mtu;
;;     IFTYPE IfType;
;;     IF_OPER_STATUS OperStatus;
;;     IF_INDEX Ipv6IfIndex;
;;     ULONG ZoneIndices[16];
;;     PIP_ADAPTER_PREFIX_XP FirstPrefix;

;;     ULONG64 TransmitLinkSpeed;
;;     ULONG64 ReceiveLinkSpeed;
;;     PIP_ADAPTER_WINS_SERVER_ADDRESS_LH FirstWinsServerAddress;
;;     PIP_ADAPTER_GATEWAY_ADDRESS_LH FirstGatewayAddress;
;;     ULONG Ipv4Metric;
;;     ULONG Ipv6Metric;
;;     IF_LUID Luid;
;;     SOCKET_ADDRESS Dhcpv4Server;
;;     NET_IF_COMPARTMENT_ID CompartmentId;
;;     NET_IF_NETWORK_GUID NetworkGuid;
;;     NET_IF_CONNECTION_TYPE ConnectionType;    
;;     TUNNEL_TYPE TunnelType;
;;     //
;;     // DHCP v6 Info.
;;     //
;;     SOCKET_ADDRESS Dhcpv6Server;
;;     BYTE Dhcpv6ClientDuid[MAX_DHCPV6_DUID_LENGTH];
;;     ULONG Dhcpv6ClientDuidLength;
;;     ULONG Dhcpv6Iaid;
;; #if (NTDDI_VERSION >= NTDDI_VISTASP1)
;;     PIP_ADAPTER_DNS_SUFFIX FirstDnsSuffix;
;; #endif
;; }
(defcstruct ip-adapter-address
  (length :uint32)
  (index :uint32)
  (next :pointer)
  (name :pointer)
  (unicast-address :pointer) ;; IP_ADAPTER_UNICAST_ADDRESS_LH
  (anycast-address :pointer)
  (multicast-address :pointer)
  (dns-address :pointer)
  (dns-suffix :pointer)
  (description :pointer)
  (friendly-name :pointer)
  (physical-address :uint8 :count 8)
  (physical-address-len :uint32)
  (flags :uint32) 
  (mtu :uint32)
  (iftype :uint32)
  (status :uint32)
  (ipv6-index :uint32)
  (zone-indices :uint32 :count 16)
  (first-prefix :pointer)
  (transmit-speed :uint64)
  (receive-speed :uint64)
  (wins-server :pointer)
  (gateway-address :pointer)
  (ipv4-metric :uint32)
  (ipv6-metric :uint32)
  (luid :uint64)
  (dhcp-server :pointer)
  (dhcp-server-len :uint32)
  (compartment-id :uint32)
  (network-guid :uint8 :count 16)
  (conn-type :uint32)
  (dhcp-server6 :pointer)
  (dhcp-server6-len :uint32)
  (duid :uint8 :count 130)
  (duid-len :uint32)
  (iad :uint32)
  (dns-suffix :pointer))


;; ULONG
;; WINAPI
;; GetAdaptersAddresses(
;;     _In_ ULONG Family,
;;     _In_ ULONG Flags,
;;     _Reserved_ PVOID Reserved,
;;     _Out_writes_bytes_opt_(*SizePointer) PIP_ADAPTER_ADDRESSES AdapterAddresses,
;;     _Inout_ PULONG SizePointer
;;     );
;; this lives in Iphlpapi.dll
(define-foreign-library iphlpapi
  (:windows "Iphlpapi.dll"))

(use-foreign-library iphlpapi)

(defcfun (%get-adapter-addresses "GetAdaptersAddresses" :convention :stdcall) :uint32
  (family :uint32)
  (flags :uint32)
  (reserved :pointer)
  (buffer :pointer)
  (count :pointer))

;; struct _IP_ADDR_STRING {
;;     struct _IP_ADDR_STRING* Next;
;;     IP_ADDRESS_STRING IpAddress;
;;     IP_MASK_STRING IpMask;
;;     DWORD Context;
;; } 
(defcstruct ip-addr-string
  (next :pointer)
  (addr :uint8 :count 16)
  (mask :uint8 :count 16)
  (context :uint32))

;; struct _IP_ADDR_STRING {
;;     struct _IP_ADDR_STRING* Next;
;;     IP_ADDRESS_STRING IpAddress;
;;     IP_MASK_STRING IpMask;
;;     DWORD Context;
;; }
(defcstruct ip-addr-string
  (next :pointer)
  (addr :uint8 :count 16)
  (mask :uint8 :count 16)
  (context :uint32))

;; struct _IP_ADAPTER_INFO {
;;     struct _IP_ADAPTER_INFO* Next;
;;     DWORD ComboIndex;
;;     char AdapterName[MAX_ADAPTER_NAME_LENGTH + 4];
;;     char Description[MAX_ADAPTER_DESCRIPTION_LENGTH + 4];
;;     UINT AddressLength;
;;     BYTE Address[MAX_ADAPTER_ADDRESS_LENGTH];
;;     DWORD Index;
;;     UINT Type;
;;     UINT DhcpEnabled;
;;     PIP_ADDR_STRING CurrentIpAddress;
;;     IP_ADDR_STRING IpAddressList;
;;     IP_ADDR_STRING GatewayList;
;;     IP_ADDR_STRING DhcpServer;
;;     BOOL HaveWins;
;;     IP_ADDR_STRING PrimaryWinsServer;
;;     IP_ADDR_STRING SecondaryWinsServer;
;;     time_t LeaseObtained;
;;     time_t LeaseExpires;
;; }
(defcstruct ip-adapter-info
  (next :pointer)
  (combo-index :uint32)
  (adapter-name :uint8 :count 260)
  (description :uint8 :count 132)
  (addrlen :uint32)
  (addr :uint8 :count 8)
  (index :uint32)
  (type :uint32)
  (dhcp-enabled :uint32)
  (current-ip-address :pointer)
  (ip-address-list (:struct ip-addr-string))
  (gateway-list (:struct ip-addr-string))
  (dhcp-server (:struct ip-addr-string))
  (havewins :boolean)
  (primary-wins-server (:struct ip-addr-string))
  (secondary-wins-server (:struct ip-addr-string))
  (lease-obtained :uint64)
  (lease-expires :uint64))

;; ULONG
;; WINAPI
;; GetAdaptersInfo(
;;     _Out_writes_bytes_opt_(*SizePointer) PIP_ADAPTER_INFO AdapterInfo,
;;     _Inout_                         PULONG           SizePointer
;;     );
(defcfun (%get-adapters-info "GetAdaptersInfo" :convention :stdcall) :uint32
  (p :pointer)
  (sizep :pointer))

(defun get-adapters-info (buffer buffer-size)
  (with-foreign-object (sizep :uint32)
    (setf (mem-ref sizep :uint32) buffer-size)
    (let ((sts (%get-adapters-info buffer sizep)))
      (unless (zerop sts) (get-last-error)))
    ;; decode the list of adapter info structs
    (do ((ip buffer
             (foreign-slot-value ip
                                 '(:struct ip-adapter-info)
                                 'next))
         (rlist nil))
        ((null-pointer-p ip) rlist)
      ;; build up return structure, we just use a plist
      (let ((r (list)))
        ;; get adapter index 
        (setf (getf r :index)
              (foreign-slot-value ip
                                  '(:struct ip-adapter-info)
                                  'index))

        ;; get netmask(s)
        (do ((ipaddrs (foreign-slot-pointer ip
                                           '(:struct ip-adapter-info)
                                           'ip-address-list)
                      (foreign-slot-value ipaddrs
                                          '(:struct ip-addr-string)
                                          'next)))
            ((null-pointer-p ipaddrs) r)
          (let ((ip-string
                 (foreign-string-to-lisp (foreign-slot-pointer ipaddrs
                                                               '(:struct ip-addr-string)
                                                               'addr)))
                (mask-string
                 (foreign-string-to-lisp (foreign-slot-pointer ipaddrs
                                                               '(:struct ip-addr-string)
                                                               'mask))))
            (unless (string= mask-string "")
              ;; IpAddressList.IpMask.String
              (let ((maskaddr (dotted-quad-to-inaddr mask-string))
                    (ipaddr (dotted-quad-to-inaddr ip-string))
                    (brdaddr (make-array 4 :initial-element 0)))
                (push (sockaddr-in maskaddr 0)
                      (getf r :netmask))
                ;; compute broadcast address. brdaddr ::= ip | ~netmask
                (dotimes (i 4)
                  (setf (aref brdaddr i)
                        (mod (+ 256 (logior (aref ipaddr i) (lognot (aref maskaddr i))))
                             256)))
                (push (sockaddr-in brdaddr 0) (getf r :broadcast))))))
        
        (push r rlist)))))

      





(defun list-adapters (&key (buffer-size 32768))
  "List the local machine physical network adapters. Returns a list of ADAPTER structures."
  (with-foreign-objects ((ptr :uint8 buffer-size)
                         (count :uint32))
    (setf (mem-aref count :uint32) buffer-size)
    (let* ((adinfo (get-adapters-info ptr buffer-size)) ;; get netmasks etc 
           (sts (%get-adapter-addresses 0 0 (null-pointer) ptr count)))
      (unless (zerop sts) (get-last-error))
      (do ((iface ptr (foreign-slot-value iface '(:struct ip-adapter-address) 'next))
           (ret nil))
          ((null-pointer-p iface) ret)
        (let ((ad (make-adapter))
              (count (foreign-slot-value iface '(:struct ip-adapter-address) 'physical-address-len)))
          (setf (adapter-type ad)
                (let ((type (foreign-slot-value iface '(:struct ip-adapter-address) 'iftype)))
                  (case type
                    (131 :multi-link)
                    (24 :loopback)
                    (6 :ethernet)
		    (23 :ppp)
		    (71 :ieee80211)
                    (otherwise type)))
                (adapter-address ad)
                (make-array count))
          (dotimes (i count)
            (setf (aref (adapter-address ad) i)
                  (mem-aref (foreign-slot-value iface '(:struct ip-adapter-address) 'physical-address)
                            :uint8
                            i)))
          (setf (adapter-name ad) 
                (foreign-string-to-lisp (foreign-slot-value iface '(:struct ip-adapter-address) 'friendly-name)
                                        :encoding :utf-16le)
                (adapter-index ad)
                (foreign-slot-value iface '(:struct ip-adapter-address) 'index))

          ;; now try and get the unicast addresses
          (do ((unicast (foreign-slot-value iface '(:struct ip-adapter-address) 'unicast-address)
                        (foreign-slot-value unicast '(:struct ip-adapter-unicast-address) 'next)))
              ((null-pointer-p unicast))
            (let ((ap (foreign-slot-value unicast '(:struct ip-adapter-unicast-address) 'addr)))
              ;; ap == struct sockaddr
              (let ((family (mem-aref ap :uint16)))
                (cond
                  ((= family +af-inet+)
                   (let ((addr (mem-ref ap '(:struct sockaddr-in))))
                     (push addr (adapter-unicast ad))))
                  ((= family +af-inet6+)
                   (let ((addr (mem-ref ap '(:struct sockaddr-in6))))
                     (push addr (adapter-unicast ad))))))))
          
          ;; ;; get the anycast addresses
          ;; (do ((anycast (foreign-slot-value iface '(:struct ip-adapter-address) 'anycast-address)
          ;;               (foreign-slot-value anycast '(:struct ip-adapter-anycast-address) 'next)))
          ;;     ((null-pointer-p anycast))
          ;;   (let ((ap (foreign-slot-value anycast '(:struct ip-adapter-anycast-address) 'addr)))
          ;;     ;; ap == struct sockaddr
          ;;     (let ((family (mem-aref ap :uint16)))
          ;;       (cond
          ;;         ((= family +af-inet+)
          ;;          (let ((addr (mem-ref ap '(:struct sockaddr-in))))
          ;;            (push addr (adapter-anycast ad))))
          ;;         ((= family +af-inet6+)
          ;;          (let ((addr (mem-ref ap '(:struct sockaddr-in6))))
          ;;            (push addr (adapter-anycast ad))))))))

          ;; ;; now try and get the multicast addresses
          ;; (do ((multicast (foreign-slot-value iface '(:struct ip-adapter-address) 'multicast-address)
          ;;                 (foreign-slot-value multicast '(:struct ip-adapter-multicast-address) 'next)))
          ;;     ((null-pointer-p multicast))
          ;;   (let ((ap (foreign-slot-value multicast '(:struct ip-adapter-multicast-address) 'addr)))
          ;;     ;; ap == struct sockaddr
          ;;     (let ((family (mem-aref ap :uint16)))
          ;;       (cond
          ;;         ((= family +af-inet+)
          ;;          (let ((addr (mem-ref ap '(:struct sockaddr-in))))
          ;;            (push addr (adapter-multicast ad))))
          ;;         ((= family +af-inet6+)
          ;;          (let ((addr (mem-ref ap '(:struct sockaddr-in6))))
          ;;            (push addr (adapter-multicast ad))))))))

          ;; ;; now try and get the dnscast addresses
          ;; (do ((dns (foreign-slot-value iface '(:struct ip-adapter-address) 'dns-address)
          ;;           (foreign-slot-value dns '(:struct ip-adapter-dns-server-address) 'next)))
          ;;     ((null-pointer-p dns))
          ;;   (let ((ap (foreign-slot-value dns '(:struct ip-adapter-dns-server-address) 'addr)))
          ;;     ;; ap == struct sockaddr
          ;;     (let ((family (mem-aref ap :uint16)))
          ;;       (cond
          ;;         ((= family +af-inet+)
          ;;          (let ((addr (mem-ref ap '(:struct sockaddr-in))))
          ;;            (push addr (adapter-dns ad))))
          ;;         ((= family +af-inet6+)
          ;;          (let ((addr (mem-ref ap '(:struct sockaddr-in6))))
          ;;            (push addr (adapter-dns ad))))))))

          ;; get the mtu
          (setf (adapter-mtu ad) (foreign-slot-value iface '(:struct ip-adapter-address) 'mtu))
          
          ;; get the status
          (let ((sts (foreign-slot-value iface '(:struct ip-adapter-address) 'status)))
            (setf (adapter-status ad)
                  (case sts
                    (1 ;; IfOperStatusUp
                     :up)
                    (2 ;; IfOperStatusDown
                     :down)
                    (otherwise :unknown))))         
                    ;; (3 ;; IfOperStatusTesting
                    ;;    :testing)
                    ;; (4 ;; IfOperStatusUnknown
                    ;;    :unknown)
                    ;; (5 ;; IfOperStatusDormant
                    ;;  :dormant)
                    ;; (6 ;; IfOperStatusNotPresent
                    ;;  :not-present)
                    ;; (7 ;; IfOperStatusLowerLayerDown
                    ;;  :lower-layer-down)
                    ;; (otherwise sts))))

          ;; Get netmasks out of the info
          (let ((ainfo (find-if (lambda (info)
                                  (= (getf info :index) (adapter-index ad)))
                                adinfo)))
            (when ainfo
              (setf (adapter-netmask ad) (getf ainfo :netmask)
                    (adapter-broadcast ad) (getf ainfo :broadcast))))
          
          (push ad ret))))))


;; --------------- DNS -------------------

;; (defconstant +max-host-name-len+ 128)
;; (defconstant +max-domain-name-len+ 128)
;; (defconstant +max-scope-id-len+ 256)

;; struct {
;;     char HostName[MAX_HOSTNAME_LEN + 4] ;
;;     char DomainName[MAX_DOMAIN_NAME_LEN + 4];
;;     PIP_ADDR_STRING CurrentDnsServer;
;;     IP_ADDR_STRING DnsServerList;
;;     UINT NodeType;
;;     char ScopeId[MAX_SCOPE_ID_LEN + 4];
;;     UINT EnableRouting;
;;     UINT EnableProxy;
;;     UINT EnableDns;
;; }
(defcstruct fixed-info
  (hostname :uint8 :count 132) ;; (+ +max-hostname-len+ 4))
  (domain-name :uint8 :count 132) ;; (+ +max-domain-name+ 4))
  (current-dns-server :pointer)
  (dns-server-list (:struct ip-addr-string))
  (node-type :uint32)
  (scope-id :uint8 :count 260) ;; (+ +max-scope-id-len+ 4))
  (enable-routing :uint32)
  (enable-proxy :uint32)
  (enable-dns :uint32))

;;DWORD WINAPI GetNetworkParams( PFIXED_INFO pFixedInfo, PULONG pOutBufLen );
(defcfun (%get-network-params "GetNetworkParams" :convention :stdcall) :uint32
  (pinfo :pointer)
  (len :pointer))

(defstruct network-params
  hostname
  domain-name
  dns-servers
  node-type
  scope-id
  enable-routing
  enable-proxy
  enable-dns)

(defun get-network-params ()
  (with-foreign-objects ((ifo :uint8 2048)
                         (plen :uint32))
    (setf (mem-aref plen :uint32) 2048)
    (let ((sts (%get-network-params ifo plen))
          (np (make-network-params)))
      (unless (zerop sts) (get-last-error))

      ;; set everything except dns addresses
      (setf (network-params-hostname np)
            (foreign-string-to-lisp (foreign-slot-pointer ifo '(:struct fixed-info) 'hostname))
            (network-params-domain-name np)
            (foreign-string-to-lisp (foreign-slot-pointer ifo '(:struct fixed-info) 'domain-name))
            (network-params-node-type np)
            (foreign-slot-value ifo '(:struct fixed-info) 'node-type)
            (network-params-scope-id np)
            (foreign-string-to-lisp (foreign-slot-pointer ifo '(:struct fixed-info) 'scope-id))
            (network-params-enable-routing np)
            (foreign-slot-value ifo '(:struct fixed-info) 'enable-routing)
            (network-params-enable-proxy np)
            (foreign-slot-value ifo '(:struct fixed-info) 'enable-proxy)
            (network-params-enable-dns np)
            (foreign-slot-value ifo '(:struct fixed-info) 'enable-dns))
                  
      (do ((p (foreign-slot-pointer ifo '(:struct fixed-info) 'dns-server-list)
              (foreign-slot-value p '(:struct ip-addr-string) 'next)))
          ((null-pointer-p p))
        (let ((ap (foreign-slot-pointer p '(:struct ip-addr-string) 'addr)))
          (push (foreign-string-to-lisp ap)
                (network-params-dns-servers np))))
      
      np)))

(defun get-name-servers ()
  "Returns a list of SOCKADDR-IN addresses for configured DNS name servers."
  (let ((np (get-network-params)))
    (mapcan (lambda (ns)
              ;; convert the dotted quad to sockaddr-in structs
	      (unless (string-equal ns "")
		(list (make-sockaddr-in :addr (dotted-quad-to-inaddr ns)
				  :port 53))))
            (network-params-dns-servers np))))

(defun get-host-name ()
  "Returns (values hostname domain-name)."
  (let ((np (get-network-params)))
    (values (network-params-hostname np)
	    (network-params-domain-name np))))
