;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(defpackage #:fsocket
  (:use #:cl #:cffi)
  (:export #:open-socket
           #:close-socket
           
           #:socket-bind
           #:socket-connect
           #:socket-listen
           #:socket-accept
           #:socket-shutdown
           #:socket-name
           
           ;; socket I/O
           #:socket-send
           #:socket-sendto
           #:socket-recv
           #:socket-recvfrom

           ;; [gs]ockopt
           #:socket-option

           ;; polling 
           #:open-poll
           #:close-poll
           #:poll-register
           #:poll-unregister 
           #:poll-find 
           #:poll
           #:poll-context
           #:poll-context-fds
           
           ;; pollfd class 
           #:pollfd           

           ;; pollfd accessors and related functions
           #:pollfd-fd
           #:pollfd-events
           #:pollfd-revents
           #:poll-event
           #:poll-event-p
           #:poll-events
           #:doevents

           ;; sockaddr-in struct 
           #:sockaddr-in
           #:make-sockaddr-in
           #:sockaddr-in-p
           #:sockaddr-in-addr
           #:sockaddr-in-port

           ;; sockaddr-in6 struct 
           #:sockaddr-in6
           #:make-sockaddr-in6
           #:sockaddr-in6-p
           #:sockaddr-in6-addr
           #:sockaddr-in6-port
           #:sockaddr-in6-scope
           
           ;; address predicates 
           #:loopback-p
           #:sockaddr=
           
           ;; host network adapters
           #:list-adapters
           #:adapter
           #:adapter-name
           #:adapter-type
           #:adapter-address
           #:adapter-index
           #:adapter-unicast
           #:adapter-status
           #:adapter-mtu

           ;; conditions
           #:fsocket-error
           #:fsocket-short-buffer

           #:multicast-join 
           #:open-multicast-socket 

           ;; DNS functions
           #:get-name-servers
           #:get-host-name
	   
           ))

