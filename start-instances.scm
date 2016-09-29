#!/bin/bash
exec scsh -ll dope-deploy-special-forms.scm -ll dope-deploy-temp-dir.scm -ll dope-deploy-git.scm -ll dope-deploy-user.scm -o dope-deploy-user -o dope-deploy-special-forms -o srfi-2 -e main -s $0 "$@" # -*- mode: Scheme; -*-
!#

(define +build-name+ "hacker-news-notify-api")
(define +instance-name+ (string-append +build-name+ "-instance"))
(define +network-name+ "canton-internal")
(define +data-base+ (string-append (getenv "HOME") "/dev-data"))

(define +couchdb-instance-name+ (string-append +build-name+ "-couchdb-instance"))
(define +couchdb-data-dir+
  (string-append +data-base+ "/" +couchdb-instance-name+ "/"))

(define (main args)
  (run (docker network create --driver bridge ,+network-name+)
       (> 2 "/dev/null"))
  (or (zero?
       (run
        (docker run
         --name ,+couchdb-instance-name+
         --net ,+network-name+
         --restart always
         -v ,(string-append +couchdb-data-dir+ ":/usr/local/var/lib/couchdb:rw")
         -d
         couchdb)))
      (error "Failed to start couchdb" +couchdb-instance-name+))
  (or (zero?
       (run
        (docker run
         --name ,+instance-name+
         --net ,+network-name+
         --restart always
         -d
         ,+build-name+)))
      (error "Failed to start instance" +instance-name+)))
