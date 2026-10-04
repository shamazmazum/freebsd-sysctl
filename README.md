freebsd-sysctl
=============

`freebsd-sysctl` is a wrapper around `sysctl` system call for FreeBSD. It can be
used, for example, in system monitors in StumpWM's mode line for tracking CPU
temperature. Currently it can get/set sysctl variables, automatically detecting
their formats, list sysctl nodes, etc.

Examples
--------
Here is the documentation in a form of examples.
```lisp
;; Read a sysctl value
(freebsd-sysctl:sysctl-by-name "kern.hz")
;; (values 1000 nil)

;; You can also access values via MIBs (Management Information Base)
(freebsd-sysctl:sysctl (freebsd-sysctl:sysctl-name=>mib "kern.hz"))
;; (values 1000 nil)

;; It can detect temperature format
(freebsd-sysctl:sysctl-by-name "dev.cpu.0.temperature")
;; (values 46.149994 nil)

;; It also understands strings
(freebsd-sysctl:sysctl-by-name "dev.pcm.3.output")
;; (values "Line-Out" nil)

;; You can set a new value to sysctl
(freebsd-sysctl:sysctl-by-name "dev.pcm.3.output" "Headphones")
;; (values "Line-Out" "Headphones")

;; You can list a sysctl node
(freebsd-sysctl:list-sysctls "dev.pcm.3.play")
;; ("dev.pcm.3.play.vchans" "dev.pcm.3.play.vchanmode" "dev.pcm.3.play.vchanrate"
;;  "dev.pcm.3.play.vchanformat")

;; You can query the type of sysctl
(freebsd-sysctl:sysctl-type
  (freebsd-sysctl:sysctl-name=>mib "dev.atdma.0.%desc"))
;; :STRING
(freebsd-sysctl:sysctl-type
  (freebsd-sysctl:sysctl-name=>mib "dev.cpu.0.freq"))
;; (FREEBSD-SYSCTL:SIGNED-INTEGER 4)
(freebsd-sysctl:sysctl-type
  (freebsd-sysctl:sysctl-name=>mib "dev.cpu.0.temperature"))
;; (FREEBSD-SYSCTL:TEMPERATURE 1)
```
