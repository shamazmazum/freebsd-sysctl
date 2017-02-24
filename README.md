freebsd-sysctl
=============

`freebsd-sysctl` is a wrapper around `sysctl` system call for FreeBSD. It can be
used, for example, in system monitors in StumpWM's mode line for tracking CPU
temperature. Currently it can get/set sysctl variables, automatically detecting
their formats, list sysctl nodes, etc.

Examples
--------
Here is the documentation in a form of examples.
~~~~~~~~~~~~~~~~{.lisp}
(freebsd-sysctl:sysctl-by-name "kern.hz")
;;  >> 1000
;;  >> NIL

;; It can detect temperature format
(freebsd-sysctl:sysctl-by-name "dev.cpu.0.temperature")
;;  >> 46.149994
;;  >> NIL

;; It also understands strings
(freebsd-sysctl:sysctl-by-name "dev.pcm.3.output")
;;  >> "Line-Out"
;;  >> NIL

;; You can set a new value to sysctl
(freebsd-sysctl:sysctl-by-name "dev.pcm.3.output" "Headphones")
;;  >> "Line-Out"
;;  >> "Headphones"

;; You can list a sysctl node
(freebsd-sysctl:list-sysctls "dev.pcm.3.play")
;;  >> ("dev.pcm.3.play.vchans" "dev.pcm.3.play.vchanmode" "dev.pcm.3.play.vchanrate"
;;      "dev.pcm.3.play.vchanformat")
~~~~~~~~~~~~~~~

TODO
----
Make it possible to get all sysctls as with `sysctl -a`.
