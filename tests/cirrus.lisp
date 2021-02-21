(defun do-all()
  (ql:quickload :freebsd-sysctl/tests)
  (uiop:quit
   (if (uiop:call-function "freebsd-sysctl-tests:run-tests")
        0 1)))

(do-all)
