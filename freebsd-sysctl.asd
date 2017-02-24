(defsystem :freebsd-sysctl
    :description "Sysctl kernel control mechanism for common lisp"
    :maintainer "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :licence "2-clause BSD"
    :version "1.0"
    :depends-on (:cffi)
    :components ((:file "package")
                 (:file "freebsd-sysctl" :depends-on ("package"))))
