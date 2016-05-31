;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file defines a simple function for intercepting and forwarding TCP traffic.

(defpackage #:netcat
  (:use #:cl #:fsocket))

(in-package #:netcat)

(defun hd (buffer &key (start 0) end (stream *standard-output*))
  "Hexdump output."
  (let ((lbuff (make-array 16))
        (len (- (or end (length buffer)) start)))
    (labels ((pline (lbuff count)
               (dotimes (i count)
                 (format stream " ~2,'0X" (aref lbuff i)))
               (dotimes (i (- 16 count))
                 (format stream "   "))
               (format stream " | ")
               (dotimes (i count)
                 (let ((char (code-char (svref lbuff i))))
                   (format stream "~C" 
                           (if (graphic-char-p char) char #\.))))
               (terpri)))
      (do ((pos start (+ pos 16)))
          ((>= pos len))
        (let ((count (min 16 (- len pos))))
          (dotimes (i count)
            (setf (aref lbuff i) (aref buffer (+ pos i))))
          (format stream "; ~8,'0X:  " pos)
          (pline lbuff count))))))



;; (defclass listen-pollfd (pollfd)
;;   ())
;; (defun make-listen (fd)
;;   (make-instance 'listen-pollfd
;; 		 :fd fd
;; 		 :events (poll-events :pollin)))

;; (defclass local-pollfd (pollfd)
;;   ())
;; (defun make-local (fd)
;;   (make-instance 'local-pollfd
;; 		 :fd fd
;; 		 :events (poll-events :pollin)))
  
;; (defclass proxy-pollfd (pollfd)
;;   ())
;; (defun make-proxy (fd)
;;   (make-instance 'proxy-pollfd
;; 		 :fd fd
;; 		 :events (poll-events :pollin)))

;; (defun netcat-tcp (host port
;; 	       &key (local-port 8000) (timeout 1000) (buffer-size 4096)
;; 		 (stream *standard-output*) text-p ssl-certificate-file
;; 		 ssl-privatekey-file ssl-privatekey-password)
;;   "Run as a proxy, dumping all traffic.
;; LOCAL-PORT ::= local port to bind to 
;; TIMEOUT ::= poll timeout 
;; BUFFER-SIZE ::= buffer size 
;; STREAM ::= output stream 
;; TEXT-P ::= if true outputs text, decodes data as UTF-8 text otherwise hexdumps 
;; HOST, PORT ::= host and port to proxy reads/writes to."
;;   (let ((buffer (make-array buffer-size :element-type '(unsigned-byte 8)))
;; 	(proxy-addr (let ((addr (dns:get-host-by-name host)))
;; 		      (unless addr (error "Host ~S not found" host))
;; 		      (sockaddr-in (first addr) port))))
;;     (with-poll (pc)
;;       (with-tcp-socket (fd local-port)
;; 	(poll-register pc (make-listen fd))
;; 	(when (poll pc)
;; 	  (multiple-value-bind (lconn laddr) (socket-accept fd)
;; 	    (let ((pconn nil)
;; 		  (lstream nil)
;; 		  (pstream nil))
;; 	      (format t ";; ACCEPTED ~A~%" (sockaddr-string laddr))
;; 	      (unwind-protect
;; 		   (progn
;; 		     (poll-register pc (make-local lconn))
;; 		     (format stream ";; CONNECTING...~%")
;; 		     (setf pconn (open-tcp-connection proxy-addr))
;; 		     (format stream ";; CONNECTED ~A~%" (sockaddr-string proxy-addr))
;; 		     (poll-register pc (make-proxy pconn))
;; 		     ;; wrap connections in streams 
;; 		     (cond
;; 		       (ssl-certificate-file 
;; 			;; TODO
;; 			(format stream ";; ATTACHING SSL...~%")
;; 			(setf lstream
;; 			      (cl+ssl:make-ssl-server-stream (make-tcp-stream lconn)
;; 							     :certificate ssl-certificate-file
;; 							     :key ssl-privatekey-file
;; 							     :password ssl-privatekey-password)
;; 			      pstream
;; 			      (cl+ssl:make-ssl-client-stream (make-tcp-stream pconn))))
;; 		       (t
;; 			(setf lstream (make-tcp-stream lconn)
;; 			      pstream (make-tcp-stream pconn))))
;; 		     ;; now we read/write and proxy the I/O to the remote side 
;; 		     (do ((done nil))
;; 			 (done)
;; 		       (format stream ";; POLLING~%")
;; 		       (doevents (pfd event) (let ((pfds (poll pc :timeout timeout)))
;; 					       (unless pfds
;; 						 (format stream ";; TIMEOUT~%")
;; 						 (setf done t))
;; 					       pfds)
;; 			 (format t ";; PFD ~A EVENT ~S~%" (type-of pfd) event)
;; 			 (typecase pfd
;; 			   (local-pollfd 
;; 			    (case event
;; 			      (:pollin ;; ready to recv from the local connection			
;; 			       (let ((cnt (read-sequence buffer lstream) #+nil(socket-recv lconn buffer)))
;; 				 (cond
;; 				   ((zerop cnt)
;; 				    (format stream ";; LOCAL shutdown~%")
;; 				    (setf done t))
;; 				   (t 
;; 				    (format stream ";; ~A LOCAL ~A -> REMOTE ~A~%"
;; 					    cnt (sockaddr-string laddr) (sockaddr-string proxy-addr))
;; 				    (if text-p
;; 					(format stream "~A~%" (babel:octets-to-string buffer :end cnt))
;; 					(hd buffer :end cnt :stream stream))
;; 				    ;; TODO: check for short write 
;; 				    (write-sequence buffer pstream :end cnt) #+nil(socket-send pconn buffer :end cnt)))))
;; 			      ((:pollerr :pollhup) ;; local connection closed
;; 			       (format stream ";; LOCAL closed~%")
;; 			       (setf done t))))
;; 			   (proxy-pollfd		     
;; 			    (case event
;; 			      (:pollin ;; ready to recv from proxy
;; 			       (let ((cnt (read-sequence buffer pstream) #+nil(socket-recv pconn buffer)))
;; 				 (cond
;; 				   ((zerop cnt)
;; 				    (format stream ";; REMOTE shutdown~%")
;; 				    (setf done t))
;; 				   (t 
;; 				    (format stream ";; ~A REMOTE ~A -> LOCAL ~A~%"
;; 					    cnt (sockaddr-string proxy-addr) (sockaddr-string laddr))
;; 				    (if text-p
;; 					(format stream "~A~%" (babel:octets-to-string buffer :end cnt))
;; 					(hd buffer :end cnt :stream stream))
;; 				    ;; TODO: check for short write 
;; 				    (write-sequence buffer lstream :end cnt) #+nil(socket-send lconn buffer :end cnt)))))
;; 			      ((:pollerr :pollhup)
;; 			       (format stream ";; REMOTE closed~%")
;; 			       (setf done t))))))))
;; 		(close-socket lconn)
;; 		(close-socket pconn)))))))))

			   


;; what we want:
;; 1. listens on a set of TCP ports 
;; 2. accepts connection(s) on these ports and immediately attempts to
;; connect to a designated remote address.
;; 3. all recvs from the inbound connection are sent to the designcated outbound
;; address and vice versa
;; 4. Additionally, it may be that we want some of these connections to be wrapped
;; in SSL streams

;; describes a listening TCP socket 
(defclass tcp-pollfd (pollfd)
  ((ssl-p :initform nil :initarg :ssl-p :reader tcp-pollfd-ssl-p)
   (port :initarg :port :reader tcp-pollfd-port)))

;; describes a UDP socket 
(defclass udp-pollfd (pollfd)
  ())

;; describes a TCP connection
(defclass conn (pollfd)
  ((buffer :initarg :buffer :reader conn-buffer
	   :documentation "Buffer to receive I/O.")
   (stream :initarg :stream :accessor conn-stream
	   :documentation "A stream for I/O on the connection.")
   (raddr :initarg :raddr :reader conn-raddr
	  :documentation "Remote address of connection.")
   (out :initform nil :accessor conn-out
	:documentation "A CONN instance for the outbound connection.")))

(defun make-conn-pair (fd-in fd-out ssl-p pc &key raddr oaddr certfile pkfile pkpasswd buffer-size)
  (let ((conn-in (make-instance 'conn
				:fd fd-in
				:events (poll-events :pollin)
				:buffer (make-array buffer-size :element-type '(unsigned-byte 8))
				:raddr raddr
				:stream (make-tcp-stream fd-in pc)))
	(conn-out (make-instance 'conn
				 :fd fd-out 
				 :events (poll-events :pollin)
				 :buffer (make-array buffer-size :element-type '(unsigned-byte 8))
				 :raddr oaddr 
				 :stream (make-tcp-stream fd-out pc))))
    (when ssl-p 
      (setf (conn-stream conn-in)
	    (cl+ssl:make-ssl-server-stream (conn-stream conn-in)
					   :certificate certfile 
					   :key pkfile 
					   :password pkpasswd)
	    (conn-stream conn-out)
	    (cl+ssl:make-ssl-client-stream (conn-stream conn-out))))
    (values conn-in conn-out)))

(defun register-listeners (pc mappings)
  (dolist (mapping mappings)
    (destructuring-bind (listen-port proxy-port &optional ssl-p) mapping
      (let ((fd (open-tcp-socket listen-port)))
	(poll-register pc
		       (make-instance 'tcp-pollfd 
				      :fd fd
				      :events (poll-events :pollin)
				      :port proxy-port 
				      :ssl-p ssl-p))))))

(defun close-all-sockets (pc)
  (dolist (pfd (poll-context-fds pc))
    (close-socket (pollfd-fd pfd))))

(defun netcat (mappings &key host ssl-certificate-file ssl-privatekey-file ssl-privatekey-password
			  (out-stream *standard-output*) text-p (buffer-size 4096)
			  (timeout 1000) (max-iterations 1000) verbose-p)
  "Accept connections and forward all traffic, printing traffic data. 
MAPPINGS ::= list of forms (local-port remote-port &optional ssl-p)
where LOCAL-PORT ::= local port to listen for connections.
      REMOTE-PORT ::= port on HOST to send data to.
      SSL-P ::= if true the connections is wrapped with a CL+SSL stream.
HOST ::= remote host to send data to, defaults to localhost.
SSL-CERTIFICATE-FILE, SSL-PRIVATEKEY-FILE, SSL-PRIVATEKEY-PASSWORD ::= these must be 
provided if you require using SSL-P to be true.
OUT-STREAM ::= stream to print the intercepted data to.
TEXT-P ::= if true, the data is interpreted as text to be printed, otherwise a hexdump is printed.
BUFFER-SIZE  ::= maximum recv size 
TIMEOUT ::= milliseconds to poll for."
  (with-poll (pc)
    (unwind-protect
	 (let ((inaddr (if host (dns:get-host-by-name host) #(127 0 0 1))))
	   (register-listeners pc mappings)
	   (do ((i 0 (1+ i)))
	       ((> i max-iterations))
	     (doevents (pfd event) (poll pc :timeout timeout)
	       (typecase pfd
		 (tcp-pollfd
		  (case event
		    (:pollin ;; ready to accept connection
		     (multiple-value-bind (fd-in raddr) (socket-accept (pollfd-fd pfd))
		       (when verbose-p (format out-stream ";; ACCEPT LOCAL ~A REMOTE ~A~%"
					       (sockaddr-string (socket-name (pollfd-fd pfd)))
					       (sockaddr-string raddr)))
		       (let ((oaddr (sockaddr-in inaddr (tcp-pollfd-port pfd))))
			 (when verbose-p (format out-stream ";; CONNECTING ~A ...~%" (sockaddr-string oaddr)))
		       (let ((fd-out (open-tcp-connection oaddr)))
			 (when verbose-p (format out-stream ";; CONNECTED~%"))
			 (multiple-value-bind (conn-in conn-out) (make-conn-pair fd-in fd-out (tcp-pollfd-ssl-p pfd) pc
										 :raddr raddr
										 :oaddr oaddr 
										 :certfile ssl-certificate-file
										 :pkfile ssl-privatekey-file
										 :pkpasswd ssl-privatekey-password
										 :buffer-size buffer-size)
			   (poll-register pc conn-in)
			   (poll-register pc conn-out))))))))
		 (conn
		  (case event
		    (:pollin ;; ready to recv
		     (when verbose-p (format out-stream ";; RECVING from ~A~%" (sockaddr-string (conn-raddr pfd))))
		     (let ((cnt (read-sequence (conn-buffer pfd) (conn-stream pfd))))
		       (format out-stream ";; RECV ~A from ~A~%" cnt (sockaddr-string (conn-raddr pfd)))
		       (if text-p
			   (format out-stream "~A~%" (babel:octets-to-string (conn-buffer pfd) :end cnt))
			   (hd (conn-buffer pfd) :end cnt :stream out-stream))
		       (when verbose-p (format out-stream ";; SENDING ~A to ~A ...~%" cnt (sockaddr-string (conn-raddr (conn-out pfd)))))
		       (let ((c (write-sequence (conn-buffer pfd) (conn-stream (conn-out pfd)) :end cnt)))
			 (when verbose-p (format out-stream ";; SENT ~A~%" c)))))))))))
      (close-all-sockets pc))))
			  


;; -----------------------------------------------

(defclass in-pollfd (pollfd)
  ())
(defclass out-pollfd (pollfd)
  ())

(defun netcat (local-port proxy-address &key (buffer-size 4096))
  (with-poll (pc)
    (with-tcp-socket (fd local-port)
      (multiple-value-bind (conn raddr) (socket-accept fd)
	(format t "ACCEPT ~A~%" (sockaddr-string raddr))
	(with-socket (conn)
	  (with-tcp-connection (proxy proxy-address)
	    (poll-register pc
			   (make-instance 'in-pollfd
					  :fd conn
					  :events (poll-events :pollin)))
	    (poll-register pc
			   (make-instance 'out-pollfd
					  :fd proxy
					  :events (poll-events :pollin)))
	    
	    (do ((done nil)
		 (buffer (make-array buffer-size
				     :element-type '(unsigned-byte 8))))
		(done)
	      (doevents (pfd event) (poll pc :timeout 1000)
		(format t "PFD ~A EVENT ~S~%" (class-name (class-of pfd)) event)
		(etypecase pfd
		  (in-pollfd
		   (case event
		     (:pollin ;; ready to recv from local 
		      (let ((cnt (socket-recv (pollfd-fd pfd) buffer)))
			(format t "RECV ~A ~A -> ~A~%"
				cnt (sockaddr-string raddr) (sockaddr-string proxy-address))
			(hd buffer :end cnt)
			(socket-send proxy buffer :end cnt)))
		     (:pollhup
		      (format t "IN CLOSED~%")
		      (setf done t))
		     (:pollerr
		      (format t "IN ERROR~%")
		      (setf done t))))
		  (out-pollfd
		   (case event
		     (:pollin ;; ready to recv from proxy
		      (let ((cnt (socket-recv (pollfd-fd pfd) buffer)))
			(format t "RECV ~A ~A -> ~A~%"
				cnt (sockaddr-string proxy-address) (sockaddr-string raddr))
			(hd buffer :end cnt)
			(socket-send proxy buffer :end cnt)))
		     (:pollhup
		      (format t "OUT CLOSED~%"))
		     (:pollerr
		      (format t "OUT ERROR~%")))))))))))))
	    

      
