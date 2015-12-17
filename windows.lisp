;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file defines all the functions for Windows

(in-package #:fsocket)

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

(defun open-socket (&key (family +af-inet+) (type :datagram) protocol)
  "Open a socket. Call CLOSE-SOCKET to free resources.
FAMILY ::= address family integer. Defaults to AF_INET i.e. IPv4.
TYPE ::= socket type name, defaults to SOCK_DGRAM. Can be :datagram or :stream.
PROTOCOL ::= socket protocol integer. 

Returns the socket or errors on failure."
  (declare (type integer family)
           (type symbol type)
           (type (or null integer) protocol))
  (let ((s (%socket family
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
  (etypecase addr
    (sockaddr-in
     (with-foreign-object (a '(:struct sockaddr-in))
       (setf (mem-aref a '(:struct sockaddr-in)) addr)
       (let ((sts (%bind sock
                         a
                         (foreign-type-size '(:struct sockaddr-in)))))
         (if (= sts +socket-error+)
             (get-last-error)
             nil))))
    (sockaddr-in6
     (with-foreign-object (a '(:struct sockaddr-in6))
       (setf (mem-aref a '(:struct sockaddr-in6)) addr)
       (let ((sts (%bind sock
                         a
                         (foreign-type-size '(:struct sockaddr-in6)))))
         (if (= sts +socket-error+)
             (get-last-error)
             nil))))))


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
ADDR ::= remote address."
  (etypecase addr
    (sockaddr-in
     (with-foreign-object (a '(:struct sockaddr-in))
       (setf (mem-aref a '(:struct sockaddr-in)) addr)
       (let ((sts (%connect sock a (foreign-type-size '(:struct sockaddr-in)))))
         (if (= sts +socket-error+)
             (get-last-error)
             nil))))
    (sockaddr-in6
     (with-foreign-object (a '(:struct sockaddr-in6))
       (setf (mem-aref a '(:struct sockaddr-in6)) addr)
       (let ((sts (%connect sock a (foreign-type-size '(:struct sockaddr-in6)))))
         (if (= sts +socket-error+)
             (get-last-error)
             nil))))))

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
ADDR ::= address of the connected socket."
  (with-foreign-objects ((buffer :uint8 32)
                         (alen :uint32))
    (setf (mem-aref alen :uint32) 32)
    (let ((sts (%accept sock
                        buffer
                        alen)))
      (cond
        ((invalid-socket-p sts)
         (get-last-error))
        (t
         (let ((family (mem-aref buffer :uint16)))
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
(defun socket-send (sock buffer &key (start 0) end)
  "Send a buffer on the connected socket.
SOCK ::= connected socket.
BUFFER ::= octet array.
START ::= start index of buffer.
END ::= end index of buffer.

Returns the number of bytes actually sent, which can be less than the requested length."
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
(defun socket-sendto (sock buffer addr &key (start 0) end)
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
(defun socket-recv (sock buffer &key (start 0) end)
  "Receive data from the socket.
SOCK ::= socket.
BUFFER ::= octet array to receive data into.
START ::= buffer start idnex.
END ::= buffer end index.

Retuns the number of bytes actually received, which can be less than the number requested."
  (let ((count (- (or end (length buffer)) start)))
    (with-foreign-object (p :uint8 count)
      (let ((sts (%recv sock p count 0)))
        (cond
          ((= sts +socket-error+)
           (let ((ecode (%get-last-error)))
             (if (= ecode +wsa-error-msgsize+) 
                 ;; msg larger than buffer, bytes dropped!
                 (progn (signal (make-condition 'fsocket-short-buffer))
                        count)
                 (get-last-error ecode))))
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
  (let ((count (- (or end (length buffer)) start)))
    (with-foreign-objects ((p :uint8 count)
                           (a :uint8 32)
                           (alen :uint32))
      (setf (mem-aref alen :uint32) 32)
      (let ((sts (%recvfrom sock
                            p
                            count
                            0
                            a
                            alen)))
        (cond
          ((= sts +socket-error+)
           (let ((ecode (%get-last-error)))
             (if (= ecode +wsa-error-msgsize+) 
                 ;; msg larger than buffer, bytes dropped!
                 (progn (signal (make-condition 'fsocket-short-buffer))
                        (let ((family (mem-aref a :uint16)))
                          (cond
                            ((= family +af-inet+)
                             (values count (mem-aref a '(:struct sockaddr-in))))
                            ((= family +af-inet6+)
                             (values count (mem-aref a '(:struct sockaddr-in6))))
                            (t (error 'fsocket-error :msg (format nil "Unknown address family ~A" family))))))
                 (get-last-error ecode))))
          (t
           (dotimes (i sts)
             (setf (aref buffer (+ start i)) (mem-ref p :uint8 i)))
           (let ((family (mem-aref a :uint16)))
             (cond
               ((= family +af-inet+)
                (let ((addr (mem-aref a '(:struct sockaddr-in))))
                  (values sts addr)))
               ((= family +af-inet6+)
                (let ((addr (mem-aref a '(:struct sockaddr-in6))))
                  (values sts addr)))
               (t (error 'fsocket-error :msg (format nil "Unknown address family ~A" family)))))))))))


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
                      (3 :pollin) ;; FD_ACCEPT. We map this to pollin to preserve semantics with posix poll
                      #+nil(4 :connect) ;; FD_CONNECT 
                      #+nil(5 :close)) ;; FD_CLOSE 
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

(defun poll-unregister (pc pollfd)
  "Unregister a pollfd descriptor from the poll context."
  (declare (type poll-context pc)
	   (type pollfd pollfd))

  ;; all we have to do is remove it from the list of descriptors.
  ;; We assume the user has called close-socket on the fd
  (setf (poll-context-fds pc)
        (remove pollfd (poll-context-fds pc)))
  
  nil)

;;(defconstant +wsa-wait-event-0+ 0)
(defconstant +wsa-wait-timeout+ 258)

(defun poll (pc &key timeout)
  "Poll the sockets registered with the poll context for network events.

PC ::= poll context as returne from OPEN-POLL.
TIMEOUT ::= if supplied time in milliseconds to wait. If not supplied defaults to infinity.

Returns a list of registered pollfd structures. Users should check the REVENTS slot for pending events.
"
  (declare (type poll-context pc)
           (type (or null integer) timeout))

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

(defun list-adapters (&key (buffer-size 32768))
  "List the local machine physical network adapters. Returns a list of ADAPTER structures."
  (with-foreign-objects ((ptr :uint8 buffer-size)
                         (count :uint32))
    (setf (mem-aref count :uint32) buffer-size)
    (let ((sts (%get-adapter-addresses 0 0 (null-pointer) ptr count)))
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
                     
          (push ad ret))))))


;; --------------- DNS -------------------

;; struct hostent {
;;   char FAR      *h_name;
;;   char FAR  FAR **h_aliases;
;;   short         h_addrtype;
;;   short         h_length;
;;   char FAR  FAR **h_addr_list;
;; }
;; (defcstruct hostent 
;;   (name :pointer)
;;   (aliases :pointer)
;;   (addrtype :int16)
;;   (len :int16)
;;   (addrs :pointer))

;; struct hostent* FAR gethostbyname(
;;   _In_ const char *name
;; );
;; (defcfun (%gethostbyname "gethostbyname")
;;     :pointer
;;   (name :pointer))

;; (defun get-host-by-name (name)
;;   "Lookup the known addresses for the host named NAME. Returns a list of SOCKADDR-IN or SOCKADDR-IN6 structures."
;;   (with-foreign-string (n name)
;;     (let ((hent (%gethostbyname n)))
;;       (cond
;; 	((null-pointer-p hent)
;; 	 (get-last-error))
;; 	(t 	 
;; 	 (do ((addrs nil)
;; 	      (family (foreign-slot-value hent '(:struct hostent) 'addrtype))
;; 	      (ads (foreign-slot-value hent '(:struct hostent) 'addrs))
;; 	      (i 0 (1+ i)))
;; 	     ((null-pointer-p (mem-aref ads :pointer i))
;; 	      addrs)
;; 	   (let ((a (mem-aref ads :pointer i)))
;; 	     (cond
;; 	       ((= family +af-inet+)
;; 		(let ((addr (make-array 4)))
;; 		  (dotimes (i 4)
;; 		    (setf (aref addr i) (mem-aref a :uint8 i)))
;; 		  (push (make-sockaddr-in :addr addr) addrs)))
;; 	       ((= family +af-inet6+)
;; 		(let ((addr (make-array 8)))
;; 		  (dotimes (i 8)
;; 		    (setf (aref addr i) (mem-aref a :uint16 i)))
;; 		  (push (make-sockaddr-in6 :addr addr) addrs)))
;; 	       (t 
;; 		(error 'fsocket-error :msg (format nil "Unknown address family ~A" family)))))))))))
