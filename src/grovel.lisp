(in-package :freebsd-sysctl)

(include "stdlib.h")
(include "sys/types.h")
(include "sys/sysctl.h")
(include "errno.h")

(ctype size-t "size_t")

(constant (+ctl-sysctl+ "CTL_SYSCTL"))

(constant (+ctl-sysctl-name+ "CTL_SYSCTL_NAME"))
(constant (+ctl-sysctl-next+ "CTL_SYSCTL_NEXT"))
(constant (+ctl-sysctl-oidfmt+ "CTL_SYSCTL_OIDFMT"))

(constant (+enoent+ "ENOENT"))
