(define (repo-name)
  "hacker-news-notify-api")

(define (couchdb-build-image)
  (string-append (repo-name) "-couchdb"))

(define (couchdb-instance-name)
  (string-append (couchdb-build-image) "-instance"))

(define (couchdb-data-location)
  (string-append (standard-data-location) "/" (couchdb-instance-name) "/"))

(define (api-daemon-build-image)
  (string-append (repo-name) "-daemon"))

(define (api-daemon-instance-name)
  (string-append (api-daemon-build-image) "-instance"))

(define (api-daemon-config-location)
  (string-append (standard-config-location) "/" (api-daemon-instance-name) ""))

(define (production-hostname)
  "canton")
