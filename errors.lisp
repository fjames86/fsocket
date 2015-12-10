;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:fsocket)

(define-condition fsocket-error (error)
  ((msg :initform nil :initarg :msg :reader fsocket-error-msg))
  (:report (lambda (condition stream)
             (format stream "FSOCKET-ERROR ~A" (fsocket-error-msg condition)))))


(define-condition fsocket-short-buffer ()
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "Buffer for datagram receive was shorter than message, bytes lost"))))

