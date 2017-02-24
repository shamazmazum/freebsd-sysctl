(defpackage freebsd-sysctl
  (:use #:cl #:cffi)
  (:export #:sysctl-error
           #:sysctl-error-errno

           #:sysctl-name=>mib
           #:sysctl-mib=>name
           #:sysctl
           #:sysctl-by-name
           #:list-sysctls))
