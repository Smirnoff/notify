#! /bin/sh
#| # -*- Scheme -*-
exec csi -ss "$0" "$@"
|#

(use posix)

(load (string-append (get-environment-variable "DOPE_DEPLOY_LIB")))
(load-relative "repo-config.scm")

(define (main args)
  (docker-stop/false name:   (couchdb-instance-name))
  (docker-rm/error   name:   (couchdb-instance-name)
                     force?: #t)
  (docker-stop/false name:   (api-daemon-instance-name))
  (docker-rm/error   name:   (api-daemon-instance-name)
                     force?: #t))
