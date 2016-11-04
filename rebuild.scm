#! /bin/sh
#| # -*- Scheme -*-
exec csi -ss "$0" "$@"
|#

(use posix)

(load (string-append (get-environment-variable "DOPE_DEPLOY_LIB")))
(load-relative "repo-config.scm")

(define (main args)
  (or (run-standard-redirects/false (docker build -t ,(api-daemon-build-image) "."))
      (error "Failed to build image")))
