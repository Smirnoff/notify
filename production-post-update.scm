#! /bin/sh
#| # -*- Scheme -*-
exec csi -ss "$0" "$@"
|#

(use posix)

(load (string-append (get-environment-variable "DOPE_DEPLOY_LIB")))

(define (master-ref? ref)
  (string-suffix? "master" ref))

(define (main args)
  (unless (= (length args) 1) (error "Bad arguments" args))
  (let ((ref (car args)))
    (unless (master-ref? ref)
      (format #t "Not master ref, not updating: ~s\n" ref))
    (when (master-ref? ref)
      (format #t "update master ref\n")
      (call-with-temp-directory
       (lambda (temp-dir)
         (git-checkout temp-dir 'master)
         (change-directory temp-dir)
         (run-standard-redirects/false (./ensure-running.scm)))))))
