;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file shows how to pass foreign poiters instead of Lisp octet vectors.

(defpackage #:fsocket.test4
  (:use #:cl #:fsocket))

(in-package #:fsocket.test4)

(defstruct (handle (:constructor %make-handle))
  ptr
  count)

(defun alloc-handle (count)
  (%make-handle :ptr (cffi:foreign-alloc :uint8 :count count)
		:count count))
(defun free-handle (handle)
  (cffi:foreign-free (handle-ptr handle)))
(defun handle-aref (h index)
  (cffi:mem-aref (handle-ptr h) :uint8 index))
(defun (setf handle-aref) (value h index)
  (setf (cffi:mem-aref (handle-ptr h) :uint8 index) value))
(defmacro with-handle ((var count) &body body)
  `(let ((,var (alloc-handle ,count)))
     (unwind-protect (progn ,@body)
       (free-handle ,var))))

(defun hd (buffer &key (start 0) end (stream *standard-output*))
  "Hexdump output"
  (let ((lbuff (make-array 16))
        (len (- end start)))
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
            (setf (aref lbuff i) (cffi:mem-aref buffer :uint8 (+ pos i))))
          (format stream "; ~8,'0X:  " pos)
          (pline lbuff count))))))

(defun test-send (port)
  (with-handle (h 1024)
    (with-udp-socket (fd)
      (dotimes (i 16)
	(setf (handle-aref h i) i))
      (socket-sendto fd (handle-ptr h) (sockaddr-in #(127 0 0 1) port)
		     :end 16))))

(defun test-udp (&optional port timeout)
  (with-handle (h 1024)
    (with-poll (pc)
      (with-udp-socket (fd (or port 8000))
	(format t ";; Listening on ~A~%" (socket-name fd))
	(poll-register pc (make-instance 'pollfd
					 :fd fd
					 :events (poll-events :pollin)))
	(when (poll pc :timeout (or timeout 1000))
	  (multiple-value-bind (cnt raddr) (socket-recvfrom fd (handle-ptr h) :end 1024)
	    (format t ";; RECV-FROM ~A ~A~%" cnt raddr)
	    (hd (handle-ptr h) :end cnt)))))))
