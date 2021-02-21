(in-package :freebsd-sysctl-tests)

(defun run-tests ()
  (explain! (run 'freebsd-sysctl)))

(def-suite freebsd-sysctl :description "Test freebsd-sysctl")

(in-suite freebsd-sysctl)
(test name<=>mib
  (let ((name "kern.hz"))
    (is
     (string=
      name
      (sysctl-mib=>name
       (sysctl-name=>mib name))))))
  
(test sysctl-types
  (is-true (typep (sysctl-by-name "kern.hz") 'integer))
  (is-true (typep (sysctl-by-name "kern.ostype") 'string)))

(test sysctl-list
  (is-true (not (zerop (length (list-sysctls "kern"))))))
