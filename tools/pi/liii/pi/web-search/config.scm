;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi web-search config)
  (import (scheme base)
    (scheme process-context)
    (liii os)
    (liii string)
    (liii pi read)
    (liii pi ai auth)
  ) ;import
  (export pi-resolve-google-api-key pi-resolve-search-base-url
    pi-resolve-search-proxy-url pi-resolve-search-proxy-alist
    pi-resolve-search-model-id
  ) ;export
  (begin

    (define (pi-resolve-google-api-key config opt-key)
      (cond
       ((string? opt-key)
        (let ((trimmed (string-trim-both opt-key)))
          (if (string-null? trimmed) #f trimmed)
        ) ;let
       ) ;
       ((and (pair? config)
          (let ((p (or (assoc 'provider config) (assoc "provider" config))))
            (or (not p) (equal? (cdr p) "google"))
          ) ;let
          (assoc 'api-key config)
        ) ;and
        (let ((k (cdr (assoc 'api-key config))))
          (if
            (and (string? k) (not (string-null? (string-trim-both k))))
            k
            #f
          ) ;if
        ) ;let
       ) ;
       ((get-environment-variable "GOOGLE_API_KEY")
        =>
        (lambda (v) (if (not (string-null? (string-trim-both v))) v #f))
       ) ;
       ((get-environment-variable "GEMINI_API_KEY")
        =>
        (lambda (v) (if (not (string-null? (string-trim-both v))) v #f))
       ) ;
       (else
         (catch #t
           (lambda ()
             (let* ((env-dir (get-environment-variable "PI_CODING_AGENT_DIR"))
                    (agent-dir
                      (if
                        (and (string? env-dir) (not (string-null? (string-trim-both env-dir))))
                        (pi-resolve-path env-dir (getcwd))
                        (pi-resolve-path "~/.pi/agent" (getcwd))
                      ) ;if
                    ) ;agent-dir
                    (auth (pi-load-auth agent-dir))
                   ) ;
               (pi-resolve-provider-key "google" auth)
             ) ;let*
           ) ;lambda
           (lambda _ #f)
         ) ;catch
       ) ;else
      ) ;cond
    ) ;define

    (define (pi-resolve-search-base-url config opt-url)
      (cond
       ((and (string? opt-url) (not (string-null? (string-trim-both opt-url))))
        opt-url
       ) ;
       ((get-environment-variable "GOOGLE_BASE_URL")
        =>
        (lambda (v) (if (not (string-null? (string-trim-both v))) v #f))
       ) ;
       ((get-environment-variable "GEMINI_BASE_URL")
        =>
        (lambda (v) (if (not (string-null? (string-trim-both v))) v #f))
       ) ;
       ((get-environment-variable "GF_PI_BASE_URL")
        =>
        (lambda (v) (if (not (string-null? (string-trim-both v))) v #f))
       ) ;
       ((and (pair? config)
          (let ((p (or (assoc 'provider config) (assoc "provider" config))))
            (or (not p) (equal? (cdr p) "google"))
          ) ;let
          (assoc 'model-meta config)
        ) ;and
        (let* ((meta (cdr (assoc 'model-meta config)))
               (bu (if (pair? meta) (or (assoc 'base-url meta) (assoc "baseURL" meta)) #f))
              ) ;
          (if
            (and bu (string? (cdr bu)) (not (string-null? (string-trim-both (cdr bu)))))
            (cdr bu)
            "https://generativelanguage.googleapis.com/v1beta"
          ) ;if
        ) ;let*
       ) ;
       (else "https://generativelanguage.googleapis.com/v1beta")
      ) ;cond
    ) ;define

    (define (pi-resolve-search-proxy-url config opt-proxy)
      (cond
       ((and (string? opt-proxy) (not (string-null? (string-trim-both opt-proxy))))
        opt-proxy
       ) ;
       ((and (pair? config) (assoc 'proxy-url config)) (cdr (assoc 'proxy-url config)))
       ((get-environment-variable "HTTPS_PROXY")
        =>
        (lambda (v) (if (not (string-null? (string-trim-both v))) v #f))
       ) ;
       ((get-environment-variable "ALL_PROXY")
        =>
        (lambda (v) (if (not (string-null? (string-trim-both v))) v #f))
       ) ;
       (else #f)
      ) ;cond
    ) ;define

    (define (pi-resolve-search-proxy-alist proxy-url)
      (if
        (and (string? proxy-url) (not (string-null? (string-trim-both proxy-url))))
        (list (cons "http" proxy-url) (cons "https" proxy-url))
        '()
      ) ;if
    ) ;define

    (define (pi-resolve-search-model-id config opt-model)
      (cond
       ((and (string? opt-model) (not (string-null? (string-trim-both opt-model))))
        opt-model
       ) ;
       ((and (pair? config)
          (assoc 'model-id config)
          (string? (cdr (assoc 'model-id config)))
        ) ;and
        (cdr (assoc 'model-id config))
       ) ;
       (else "gemini-2.5-flash")
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
