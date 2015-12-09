

(defpackage #:fsocket
  (:use #:cl #:cffi)
  (:export #:open-socket
           #:close-socket
           
           #:socket-bind
           #:socket-connect
           #:socket-listen
           #:socket-accept
           #:socket-shutdown
           
           #:socket-send
           #:socket-sendto
           #:socket-recv
           #:socket-recvfrom
           
           #:socket-option

           #:open-poll
           #:close-poll
           #:poll-register
           #:poll-unregister 
           #:poll
           #:poll-context
           #:poll-context-fds
           
           ;; other structures we might need
           #:make-pollfd                      
           #:pollfd
           
           #:datagram-pollfd
           #:make-datagram-pollfd
           #:stream-pollfd
           #:make-stream-pollfd           
           #:listening-stream-pollfd
           #:make-listening-stream-pollfd
           
           #:pollfd-fd
           #:pollfd-events
           #:pollfd-revents
           #:poll-event
           #:poll-event-p
           #:poll-events
           #:doevents
           
           #:make-sockaddr-in
           #:sockaddr-in
           #:sockaddr-in-addr
           #:sockaddr-in-port

           #:make-sockaddr-in6
           #:sockaddr-in6
           #:sockaddr-in6-addr
           #:sockaddr-in6-port
           #:sockaddr-in6-scope
           
           #:list-adapters
           
           ;; TODO
           ;; get-host-by-name
           ;; get-host-address
           ))

