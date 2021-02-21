(defsystem :freebsd-sysctl
  :description "Sysctl kernel control mechanism for common lisp"
  :maintainer "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :version "1.0"
  :defsystem-depends-on (:cffi-grovel)
  :depends-on (:cffi)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:cffi-grovel-file "grovel")
               (:file "freebsd-sysctl"))
  :in-order-to ((test-op (load-op "freebsd-sysctl/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:call-function "freebsd-sysctl-tests:run-tests")))

(defsystem :freebsd-sysctl/tests
  :name :freebsd-sysctl/tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "2-clause BSD"
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests"))
  :depends-on (:freebsd-sysctl :fiveam))
