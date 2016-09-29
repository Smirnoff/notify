#!/bin/bash
exec scsh -ll dope-deploy-special-forms.scm -ll dope-deploy-temp-dir.scm -ll dope-deploy-git.scm -o dope-deploy-special-forms -o dope-deploy-git -o dope-deploy-temp-dir -o srfi-2 -e main -s $0 "$@" # -*- mode: Scheme; -*-
!#

(define (master-ref? ref)
  (string-suffix? "master" ref))

(define (main args)
  (unless (= (length args) 2) (error "Bad arguments" args))
  (let ((script (car args))
        (ref    (cadr args)))
    (unless (master-ref? ref)
      (format #t "Not master ref, not updating: ~s\n" ref))
    (when (master-ref? ref)
      (format #t "update master ref\n")
      (call-with-temp-directory
       (lambda (temp-dir)
         (git-checkout temp-dir 'master)
         (chdir temp-dir)
         (run (./ensure-running.scm)))))))
