#! /bin/sh
#| # -*- Scheme -*-
exec csi -ss "$0" "$@"
|#

(use posix)

(load (string-append (get-environment-variable "DOPE_DEPLOY_LIB")))
(load-relative "repo-config.scm")

(define (main args)
  (docker-ensure-network! name: (machine-wide-docker-network))
  (docker-run/error
   name:    (couchdb-instance-name)
   net:     (machine-wide-docker-network)
   restart: 'always
   detach?: #t
   volumes: (list (volume-mount source:      (couchdb-data-location)
                                destination: "/usr/local/var/lib/couchdb"
                                access:      "rw"))
   image:   'couchdb)
  (docker-run/error
   name:    (api-daemon-instance-name)
   net:     (machine-wide-docker-network)
   restart: 'always
   detach?: #t
   volumes: (list (volume-mount source:      (api-daemon-config-location)
                                destination: "/home/opam/config"
                                access:      "ro"))
   image:   (api-daemon-build-image)))
