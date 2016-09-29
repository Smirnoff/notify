#!/bin/bash
exec scsh -ll dope-deploy-special-forms.scm -ll dope-deploy-temp-dir.scm -ll dope-deploy-git.scm -o dope-deploy-special-forms -o srfi-2 -e main -s $0 "$@" # -*- mode: Scheme; -*-
!#

(define (main args)
  ;; rebuild
  (run (./rebuild.scm "."))

  ;; restart
  (run (./stop-instances.scm))
  (run (./start-instances.scm)))
