#!/bin/bash
exec scsh -ll dope-deploy-special-forms.scm -ll dope-deploy-temp-dir.scm -ll dope-deploy-git.scm -o dope-deploy-special-forms -o srfi-2 -e main -s $0 "$@" # -*- mode: Scheme; -*-
!#

(define +build-name+ "hacker-news-notify-api")
(define +instance-name+ (string-append +build-name+ "-instance"))

(define +couchdb-instance-name+ (string-append +build-name+ "-couchdb-instance"))

(define (main args)
  (run (docker stop ,+couchdb-instance-name+) (> 2 "/dev/null"))
  (run (docker rm -f ,+couchdb-instance-name+) (> 2 "/dev/null"))
  (run (docker stop ,+instance-name+) (> 2 "/dev/null"))
  (run (docker rm -f ,+instance-name+) (> 2 "/dev/null")))
