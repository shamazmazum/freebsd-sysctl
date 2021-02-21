(defsystem :freebsd-sysctl
  :description "Sysctl kernel control mechanism for common lisp"
  :maintainer "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :version "1.0"
  :defsystem-depends-on (:cffi-grovel)
  :depends-on (:cffi)
  :serial t
  :components ((:file "package")
               (:cffi-grovel-file "grovel")
               (:file "freebsd-sysctl")))
