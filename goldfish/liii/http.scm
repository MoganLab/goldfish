;;
;; COPYRIGHT: (C) 2025  Liii Network Inc
;; All rights reverved.
;;

(define-library (liii http)
(import (liii hash-table)
        (liii alist)
        (liii error)
) ;import
(export http-head http-get http-post http-multipart-post http-ok?
        http-stream-get http-stream-post
        http-async-get http-async-post http-async-head http-poll http-wait-all
) ;export
(begin

(define (http-ok? r)
  (let ((status-code (r 'status-code))
        (reason (r 'reason))
        (url (r 'url)))
    (cond ((and (>= status-code 400) (< status-code 500))
           (error 'http-error
             (string-append (number->string status-code)
                            " Client Error: " reason " for url: " url)
             ) ;string-append
           ) ;error
          ((and (>= status-code 500) (< status-code 600))
           (error 'http-error
             (string-append (number->string status-code)
                            " Server Error: " reason " for url: " url
             ) ;string-append
           ) ;error
          ) ;
          (else #t)
    ) ;cond
  ) ;let
) ;define

(define (http-require-string who field value)
  (when (not (string? value))
    (type-error (string-append who ": " field " must be string") value)
  ) ;when
  value
) ;define

(define (http-require-procedure who field value)
  (when (not (procedure? value))
    (type-error (string-append who ": " field " must be a procedure") value)
  ) ;when
  value
) ;define

(define (http-scalar->string who field value)
  (cond ((string? value) value)
        ((symbol? value) (symbol->string value))
        ((or (integer? value) (real? value)) (number->string value))
        (else
          (type-error
            (string-append who ": " field " must be a string, symbol, or number")
            value
          ) ;type-error
        ) ;else
  ) ;cond
) ;define

(define (http-normalize-string-alist-entry who field entry)
  (when (not (pair? entry))
    (type-error (string-append who ": " field " entries must be key/value pairs") entry)
  ) ;when
  (when (pair? (cdr entry))
    (type-error (string-append who ": " field " entries must be key/value pairs") entry)
  ) ;when
  (cons (http-scalar->string who (string-append field " key") (car entry))
        (http-scalar->string who (string-append field " value") (cdr entry))
  ) ;cons
) ;define

(define (http-normalize-string-alist who field entries)
  (when (not (alist? entries))
    (type-error (string-append who ": " field " must be an association list") entries)
  ) ;when
  (map (lambda (entry)
         (http-normalize-string-alist-entry who field entry)
       ) ;lambda
       entries
  ) ;map
) ;define

(define http-multipart-part-keys
  '("name" "value" "file" "filename" "content-type")
) ;define

(define (http-normalize-part-key who key)
  (cond ((string? key) key)
        ((symbol? key) (symbol->string key))
        (else
          (type-error
            (string-append who ": multipart part key must be string or symbol")
            key
          ) ;type-error
        ) ;else
  ) ;cond
) ;define

(define (http-normalize-multipart-entry who entry)
  (when (not (pair? entry))
    (type-error (string-append who ": multipart part entries must be key/value pairs") entry)
  ) ;when
  (when (pair? (cdr entry))
    (type-error (string-append who ": multipart part entries must be key/value pairs") entry)
  ) ;when
  (let* ((key (http-normalize-part-key who (car entry)))
         (value (cdr entry)))
    (when (not (member key http-multipart-part-keys string=?))
      (value-error (string-append who ": multipart part contains unsupported key") key)
    ) ;when
    (cond ((or (string=? key "file")
               (string=? key "filename")
               (string=? key "content-type"))
           (when (not (string? value))
             (type-error (string-append who ": multipart part " key " must be string") value)
           ) ;when
           (cons key value)
          ) ;cond
          (else
            (cons key
                  (http-scalar->string who (string-append "multipart part " key) value)
            ) ;cons
          ) ;else
    ) ;cond
  ) ;let*
) ;define

(define (http-part-ref part key)
  (let ((entry (assoc key part string=?)))
    (and entry (cdr entry))
  ) ;let
) ;define

(define (http-normalize-multipart-part who part)
  (when (not (alist? part))
    (type-error (string-append who ": multipart part must be an association list") part)
  ) ;when
  (let* ((normalized (map (lambda (entry)
                            (http-normalize-multipart-entry who entry)
                          ) ;lambda
                          part
                     ) ;map
         )
         (name (http-part-ref normalized "name"))
         (value (http-part-ref normalized "value"))
         (file (http-part-ref normalized "file")))
    (when (or (not name) (= (string-length name) 0))
      (value-error (string-append who ": multipart part requires a non-empty name") part)
    ) ;when
    (when (or (and value file)
              (and (not value) (not file)))
      (value-error (string-append who ": multipart part must contain exactly one of value or file") part)
    ) ;when
    (when (and file (not (file-exists? file)))
      (value-error (string-append who ": multipart part file does not exist") file)
    ) ;when
    normalized
  ) ;let*
) ;define

(define (http-normalize-multipart-parts who parts)
  (when (not (list? parts))
    (type-error (string-append who ": parts must be a list") parts)
  ) ;when
  (when (null? parts)
    (value-error (string-append who ": parts must not be empty") parts)
  ) ;when
  (map (lambda (part)
         (http-normalize-multipart-part who part)
       ) ;lambda
       parts
  ) ;map
) ;define

(define* (http-head url)
  (let ((r (g_http-head (http-require-string "http-head" "url" url))))
        r
  ) ;let
) ;define*

(define* (http-get url (params '()) (headers '()) (proxy '()))
  (let* ((url (http-require-string "http-get" "url" url))
         (params (http-normalize-string-alist "http-get" "params" params))
         (headers (http-normalize-string-alist "http-get" "headers" headers))
         (proxy (http-normalize-string-alist "http-get" "proxy" proxy))
         (r (g_http-get url params headers proxy)))
        r
  ) ;let
) ;define*

(define* (http-post url (params '()) (data "") (headers '()) (proxy '()))
  (let* ((url (http-require-string "http-post" "url" url))
         (params (http-normalize-string-alist "http-post" "params" params))
         (data (http-require-string "http-post" "data" data))
         (headers (http-normalize-string-alist "http-post" "headers" headers))
         (proxy (http-normalize-string-alist "http-post" "proxy" proxy)))
    (cond ((and (> (string-length data) 0) (null? headers))
           (g_http-post url params data '(("Content-Type" . "text/plain")) proxy))
          (else (g_http-post url params data headers proxy))
    ) ;cond
  ) ;cond
) ;define*

(define* (http-multipart-post url parts (params '()) (headers '()) (proxy '()))
  (let ((url (http-require-string "http-multipart-post" "url" url))
        (parts (http-normalize-multipart-parts "http-multipart-post" parts))
        (params (http-normalize-string-alist "http-multipart-post" "params" params))
        (headers (http-normalize-string-alist "http-multipart-post" "headers" headers))
        (proxy (http-normalize-string-alist "http-multipart-post" "proxy" proxy)))
    (g_http-multipart-post url parts params headers proxy)
  ) ;let
) ;define*

;; Streaming API wrapper functions

(define* (http-stream-get url callback (userdata '()) (params '()) (proxy '()))
  (let ((url (http-require-string "http-stream-get" "url" url))
        (callback (http-require-procedure "http-stream-get" "callback" callback))
        (params (http-normalize-string-alist "http-stream-get" "params" params))
        (proxy (http-normalize-string-alist "http-stream-get" "proxy" proxy)))
    (g_http-stream-get url params proxy userdata callback)
  ) ;let
) ;define*

(define* (http-stream-post url callback (userdata '()) (params '()) (data "") (headers '()) (proxy '()))
  (let* ((url (http-require-string "http-stream-post" "url" url))
         (callback (http-require-procedure "http-stream-post" "callback" callback))
         (params (http-normalize-string-alist "http-stream-post" "params" params))
         (data (http-require-string "http-stream-post" "data" data))
         (headers (http-normalize-string-alist "http-stream-post" "headers" headers))
         (proxy (http-normalize-string-alist "http-stream-post" "proxy" proxy)))
    (cond ((and (> (string-length data) 0) (null? headers))
           (g_http-stream-post url params data '(("Content-Type" . "text/plain")) proxy userdata callback))
          (else (g_http-stream-post url params data headers proxy userdata callback))
    ) ;cond
  ) ;let*
) ;define*

;; Async HTTP API wrapper functions

(define* (http-async-get url callback (params '()) (headers '()) (proxy '()))
  (let ((url (http-require-string "http-async-get" "url" url))
        (callback (http-require-procedure "http-async-get" "callback" callback))
        (params (http-normalize-string-alist "http-async-get" "params" params))
        (headers (http-normalize-string-alist "http-async-get" "headers" headers))
        (proxy (http-normalize-string-alist "http-async-get" "proxy" proxy)))
    (g_http-async-get url params headers proxy callback)
  ) ;let
) ;define*

(define* (http-async-post url callback (params '()) (data "") (headers '()) (proxy '()))
  (let* ((url (http-require-string "http-async-post" "url" url))
         (callback (http-require-procedure "http-async-post" "callback" callback))
         (params (http-normalize-string-alist "http-async-post" "params" params))
         (data (http-require-string "http-async-post" "data" data))
         (headers (http-normalize-string-alist "http-async-post" "headers" headers))
         (proxy (http-normalize-string-alist "http-async-post" "proxy" proxy)))
    (cond ((and (> (string-length data) 0) (null? headers))
           (g_http-async-post url params data '(("Content-Type" . "text/plain")) proxy callback))
          (else (g_http-async-post url params data headers proxy callback))
    ) ;cond
  ) ;let*
) ;define*

(define* (http-async-head url callback (params '()) (headers '()) (proxy '()))
  (let ((url (http-require-string "http-async-head" "url" url))
        (callback (http-require-procedure "http-async-head" "callback" callback))
        (params (http-normalize-string-alist "http-async-head" "params" params))
        (headers (http-normalize-string-alist "http-async-head" "headers" headers))
        (proxy (http-normalize-string-alist "http-async-head" "proxy" proxy)))
    (g_http-async-head url params headers proxy callback)
  ) ;let
) ;define*

(define (http-poll)
  (g_http-poll)
) ;define

(define* (http-wait-all (timeout -1))
  (g_http-wait-all timeout)
) ;define*

) ;begin
) ;define-library
