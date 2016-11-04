#! /bin/sh
#| # -*- Scheme -*-
exec csi -ss "$0" "$@"
|#

(use posix)

(load (string-append (get-environment-variable "DOPE_DEPLOY_LIB")))

(define +ssh-login+ "brian@notify.uz")
(define +target-raw-git+ "/home/brian/projects/hacker-news-notify-api.git")
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
           (run-standard-redirects-string
            (format "ssh ~s ~s < ~s"
                    +ssh-login+
                    (format #f "cat > ~s" target-filename)
                    local-filename)))))
   +hook-files+))
