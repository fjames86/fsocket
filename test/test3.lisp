
(defpackage #:fsocket.test3
  (:use #:cl #:fsocket))

(in-package #:fsocket.test3)

(defun hd (buffer &key (start 0) end (stream *standard-output*))
  "Hexdump output"
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

(defun test-udp (&key port timeout)
  (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8))))
    (with-poll (pc)
      (with-udp-socket (fd (or port 8000))
	(format t ";; Listening on ~A~%" (socket-name fd))
	(when (poll pc :timeout (or timeout 1000))
	  (multiple-value-bind (cnt raddr) (socket-recvfrom fd buffer)
	    (format t ";; RECV-FROM ~A ~A~%" cnt raddr)
	    (hd buffer :end cnt)))))))

