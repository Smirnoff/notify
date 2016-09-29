#!/bin/bash
exec scsh -ll dope-deploy-special-forms.scm -ll dope-deploy-temp-dir.scm -ll dope-deploy-git.scm -o dope-deploy-special-forms -o srfi-2 -e main -s $0 "$@" # -*- mode: Scheme; -*-
!#

(define +build-name+ "hacker-news-notify-api")
(define +instance-name+ (string-append +build-name+ "-instance"))

(define (main args)
  (or (and-let* ((prog-name (car args))
                 (rest (cdr args))
                 (first-arg (and (pair? rest) (car rest))))
        (chdir first-arg)
        (run-or-error (docker build -t ,+build-name+ ".")
                      "Failed to build" +build-name+))
      (error "Bad args" args)))
