#! /bin/sh
#| # -*- Scheme -*-
exec csi -ss "$0" "$@"
|#

(use posix)

(load (string-append (get-environment-variable "DOPE_DEPLOY_LIB")))
(load-relative "repo-config.scm")

(define (main args)
  (run-standard-redirects/false (./rebuild.scm))
  (run-standard-redirects/false (./stop-instances.scm))
  (run-standard-redirects/false (./start-instances.scm)))
