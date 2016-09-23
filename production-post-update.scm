#! /bin/sh
exec scsh -dm -m production-post-update -e main -s $0 "$@" # -*- mode: Scheme; -*-
!#

(define-structure production-post-update
  (export main
          max-temp-dir-attempt
          temp-dir-format
          call-with-temp-directory)
  (open scheme-with-scsh srfi-34 srfi-39 (subset srfi-13 (string-suffix?))
        (with-prefix srfi-19 srfi-19:))
  (begin

(define +network-name+ "canton-internal")
(define +well-known-base+ "/tmp/well-knowns")
(define +data-base+ (string-append (getenv "HOME") "/dev-data"))

;; at some point, determine these from the repo itself
(define +build-name+ "hacker-news-notify-api")
(define +instance-name+ (string-append +build-name+ "-instance"))
(define +well-known-dir+
  (string-append +well-known-base+ "/" +instance-name+ "/.well-known"))
(define +config-source+ (string-append (getenv "HOME") "/config/" +instance-name+))

(define +couchdb-instance-name+ (string-append +build-name+ "-couchdb-instance"))
(define +couchdb-data-dir+
  (string-append +data-base+ "/" +couchdb-instance-name+ "/"))

(define-syntax unless
  (syntax-rules ()
    ((unless test body . rest)
     (if (not test) (begin body . rest)))))

(define-syntax when
  (syntax-rules ()
    ((when test body . rest)
     (if test (begin body . rest)))))

(define-syntax run-or-error
  (syntax-rules ()
    ((run-or-error pipe error-msg . rest)
     (unless (zero? (run pipe))
       (error error-msg . rest)))))

(define max-temp-dir-attempt (make-parameter 5))
(define temp-dir-format (make-parameter "/tmp/docker_temp_~a"))

(define (new-temp-dir-name)
  (format #f
          (temp-dir-format)
          (string-append
           (number->string (srfi-19:time-second (srfi-19:current-time)))
           "."
           (number->string (srfi-19:time-nanosecond (srfi-19:current-time))))))

(define (git-checkout target-dir branch)
  (with-env* `(("GIT_WORK_TREE" . ,target-dir))
    (lambda ()
      (run-or-error (git checkout -f ,branch)
                    "Failed to checkout" branch))))

(define (rec-copy-to-target source target)
  (for-each
   (lambda (filename)
     (unless (zero? (run (cp -r ,(string-append source "/" filename) ,target)))
       (error "Failed to copy file" source target filename)))
   (directory-files source)))

(define (call-with-temp-directory func)
  (define (cleanup dirname)
    (run (rm -rf ,dirname))
    #f)

  (define (procure-temp-directory count)
    (if (> count (max-temp-dir-attempt))
        (error "Failed to secure temp directory")
        (let ((dirname (new-temp-dir-name)))
          (with-exception-handler
              (lambda (exp) (procure-temp-directory (+ count 1)))
            (lambda ()
              (create-directory dirname)
              dirname)))))

  (let* ((dirname (procure-temp-directory 0)))
    (with-exception-handler
        (lambda (exp) (cleanup dirname) (raise exp))
      (lambda ()
        (let ((res (func dirname)))
          (cleanup dirname)
          res)))))

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
         (run (docker network create --driver bridge ,+network-name+))

         ;; ensuring couchdb docker instance
         (run (docker stop ,+couchdb-instance-name+))
         (run (docker rm ,+couchdb-instance-name+))
         (run (docker run --name ,+couchdb-instance-name+
                          --net ,+network-name+
                          -v ,(string-append +couchdb-data-dir+
                                             ":/usr/local/var/lib/couchdb:rw")
                          -d
                          couchdb))

         (rec-copy-to-target +config-source+ temp-dir)

         ;; building and running instance
         (unless (zero? (run (docker build -t ,+build-name+ ,temp-dir)))
           (error "Failed to build new image"))

         (run (docker stop ,+instance-name+))
         (run (docker rm ,+instance-name+))
         (run (docker run --name ,+instance-name+
                          --net ,+network-name+
                          --restart always
                          -d
                          ,+build-name+))
         #f)))))

)) ; end define-structure begin
