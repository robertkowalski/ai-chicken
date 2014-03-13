#! /usr/local/bin/csi -script

(require-extension srfi-19-core)

(use openssl http-client json rfc3339)

(define (get-user-repo-url-part)
  (let ((x (string-split (get-user-repo-combo) "/")))
    (string-append "+user:"
      (string-append (car x) (string-append "+repo:" (second x))))))


(define (get-github-url)
  (string-append "https://api.github.com/search/issues?q=commenter:"
    (string-append (get-env-username)
      (string-append "+state:open" (get-user-repo-url-part)))))


(define (get-user-repo-combo)
  (car (command-line-arguments)))

(define (get-env-username)
  (cdr (assoc "AI_GITHUB_USERNAME"  (get-environment-variables))))

; needed to filter the repos that do contain the same chars in the name,
; but are not the same
(define (get-issue-url)
  (string-append
    (string-append
      "https://github.com/" (get-user-repo-combo)) "/issues/"))


; filter the issues:
; the repos that do contain the same chars in the name,
; but are not the same (e.g. npm vs. npm-registry-client)
(define (filter-repositories j)
  (filter
    (lambda (arg)
      (if (string-contains (cdr (vector-ref arg 4)) (get-issue-url))
        #t #f)) j))


; filter the issues:
; remove all repositories that are too old
(define (filter-too-young j)
  (filter
    (lambda (arg)
      (if (date>?
          (date-subtract-duration (current-date) (make-duration #:days 14))
          (get-date (cdr (vector-ref arg 15)))) #t #f)) j))


(define (get-date rfc-date)
  (let ((v (rfc3339->vector(string->rfc3339 rfc-date))))
    ; (2014 3 7 23 41 2 0 0)
    (make-date 0 0
      (vector-ref v 4)
      (vector-ref v 3)
      (vector-ref v 2)
      (vector-ref v 1)
      (vector-ref v 0))))


(define (handlejson j)
  (let ((issues (filter-repositories (cdr (vector-ref j 1)))))
    (if (= (length issues) 0) (print "no issues found."))
    (for-each
      (lambda (a) (print (cdr (vector-ref a 4))))
      (filter-too-young issues))))


(with-input-from-request
  (get-github-url) #f (lambda ()
    (handlejson (json-read (current-input-port)))))
