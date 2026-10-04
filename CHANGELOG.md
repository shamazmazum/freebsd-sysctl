# Changelog

# Version 1.1

* Improvement: Type declarations for each exported function.
* Improvement: `sysctl-type` was exported and now returns one of the following
  supported types: `:node`, `:string`, `signed-integer`, `unsigned-integer`,
  `temperature` or `:unknown`.
* Improvement: `list-sysctls` can now accept `NIL` to list all sysctls in the
  kernel.
* Bug fix: `get-errno` was fixed but now is expected to work only with SBCL.
* Bug fix: `list-sysctls` now correctly works if the last node in the sysctl
  tree is queried.

# Version 1.0

Ability to get/set sysctls by their names/MIBs, get a list of sysctls which are
children of some specific node.
