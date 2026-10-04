(defpackage freebsd-sysctl
  (:use #:cl #:cffi)
  (:export #:sysctl-error
           #:sysctl-error-errno

           #:unsigned-integer
           #:signed-integer
           #:temperature
           #:foreign-type

           #:sysctl-name=>mib
           #:sysctl-mib=>name
           #:sysctl
           #:sysctl-by-name
           #:sysctl-type
           #:list-sysctls))
