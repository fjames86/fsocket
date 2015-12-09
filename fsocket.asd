;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :fsocket
  :name "fsocket"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "Franks socket API"
  :license "MIT"
  :serial t
  :components
  ((:file "package")
   (:file "errors")
   (:file "constants")
   (:file "common")
   #+(or win32 windows)(:file "windows")
   #-(or win32 windows)(:file "posix")
   (:file "options"))
  :depends-on (:cffi))
