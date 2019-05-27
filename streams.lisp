;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file defines a very simple gray-streams wrapper over
;;; TCP connections.

(in-package #:fsocket)

(defclass tcp-stream (trivial-gray-streams:trivial-gray-stream-mixin
		      trivial-gray-streams:fundamental-binary-input-stream 
		      trivial-gray-streams:fundamental-binary-output-stream)
  (#+cmu
   (open-p :initform t
           :accessor tcp-stream-open-p
           :documentation "For CMUCL we have to keep track of this manually.")
   (fd :initarg :fd :reader tcp-stream-fd
       :documentation "TCP connection file descriptor/handle.")))

(defun make-tcp-stream (fd)
  "Wrap a TCP connection with a gray-stream.
FD ::= TCP connection file descriptor.

Remarks: non-blocking file descriptors are not supported. This is because 
the read-byte or read-sequence functions may signal an EWOULDBLOCK status
if they are not ready for a recv operation. However, if you can ensure a 
recv would not block before calling read-byte or read-sequence functions 
then non-blocking sockets should work.

READ-BYTE and SEND-BYTE are extremely inefficient because they call directly 
to recv() and send(). You should always buffer sends and call write-sequence
whereever possible. There is almost never a reason to call read-byte, always 
call read-sequence into a buffer and wrap that in a stream."
  (make-instance 'tcp-stream :fd fd))

(defmacro with-tcp-stream ((var fd) &body body)
  "Evaluate BODY in a context where VAR is bound to the TCP file descriptor."
  `(let ((,var (make-tcp-stream ,fd)))
     ,@body))

#+:cmu
(defmethod open-stream-p ((stream mapping-stream))
  "Returns a true value if STREAM is open.  See ANSI standard."
  (tcp-stream-open-p stream))

#+:cmu
(defmethod close ((stream mapping-stream) &key abort)
  "Closes the stream STREAM.  See ANSI standard."
  (declare (ignore abort))
  (setf (tcp-stream-open-p stream) nil))

(defmethod stream-element-type ((stream tcp-stream))
  "The element type is always OCTET by definition."
  '(unsigned-byte 8))

;; use this to check if there are more bytes to read
#+nil(defmethod stream-listen ((stream mapping-stream))
  "checks whether there are bytes left to read"
  nil)

(defmethod trivial-gray-streams:stream-read-byte ((stream tcp-stream))
  (let ((b (make-array 1 :element-type '(unsigned-byte 8))))    
    (let ((cnt (socket-recv (tcp-stream-fd stream) b)))
      (if (= cnt 1)
	  (aref b 0)
	  :eof))))

(defmethod trivial-gray-streams:stream-write-byte ((stream tcp-stream) integer)
  (let ((b (make-array 1 :element-type '(unsigned-byte 8))))
    (setf (aref b 0) integer)
    (socket-send (tcp-stream-fd stream) b)))

(defmethod trivial-gray-streams:stream-read-sequence ((stream tcp-stream) seq start end &key)
  "Returns the index of last byte read."
  (declare (fixnum start end))
  (let ((n (socket-recv (tcp-stream-fd stream) seq :start start :end end)))
    #+nil(when (zerop n) (error "Connection gracefull closed"))
    (+ start n)))
        
(defmethod trivial-gray-streams:stream-write-sequence ((stream tcp-stream) seq start end &key)
  (declare (fixnum start end))
    (do ((offset start))
	((= offset end) seq)
      (let ((n (socket-send (tcp-stream-fd stream) seq :start offset :end end)))
	(incf offset n))))


(defmethod trivial-gray-streams:stream-force-output ((stream tcp-stream))
  nil)

(defmethod trivial-gray-streams:stream-finish-output ((stream tcp-stream))
  nil)

(defmethod close ((stream tcp-stream) &key abort)
  (declare (ignore abort))
  (close-socket (tcp-stream-fd stream)))
