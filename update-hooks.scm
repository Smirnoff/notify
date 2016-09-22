#! /bin/sh
exec scsh -dm -m update-hooks -e main -s $0 "$@" # -*- mode: Scheme; -*-
!#

(define-structure update-hooks
  (export main
          )
  (open scheme-with-scsh srfi-34 srfi-39
        (subset srfi-13 (string-suffix?))
        (with-prefix srfi-19 srfi-19:))
  (begin

(define +ssh-login+ "brian@notify.uz")
(define +target-raw-git+ "/home/brian/projects/hacker-news-notify-api")
(define +hooks-dir+ (string-append +target-raw-git+ "/hooks"))

(define +hook-files+
  '(("post-update" "production-post-update.scm")))

(define (main args)
  (for-each
   (lambda (entry)
     (let* ((hook-name (car entry))
            (local-filename (cadr entry))
            (target-filename (string-append +hooks-dir+ "/" hook-name)))
       (if (file-exists? local-filename)
           (run (ssh ,+ssh-login+ ,(format #f "cat > ~s" target-filename))
                (< ,local-filename)))))
   +hook-files+))

)) ;end
