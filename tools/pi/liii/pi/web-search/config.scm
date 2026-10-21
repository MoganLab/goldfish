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
    (liii pi auth)
  ) ;import
  (export pi-resolve-google-api-key pi-resolve-search-base-url
    pi-resolve-search-proxy-url pi-resolve-search-proxy-alist
    pi-resolve-search-model-id pi-valid-key?
  ) ;export
  (begin

    (define (pi-valid-key? k)
      (and (string? k) (not (string-null? (string-trim-both k))))
    ) ;define

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
          (let ((k-pair (or (assoc 'api-key config) (assoc "api-key" config))))
            (and (pair? k-pair) (pi-valid-key? (cdr k-pair)))
          ) ;let
        ) ;and
        (let ((k (cdr (or (assoc 'api-key config) (assoc "api-key" config)))))
          (string-trim-both k)
        ) ;let
       ) ;
       ((let ((v (get-environment-variable "GOOGLE_API_KEY")))
          (and (pi-valid-key? v) (string-trim-both v))
        ) ;let
        =>
        (lambda (v) v)
       ) ;
       ((let ((v (get-environment-variable "GEMINI_API_KEY")))
          (and (pi-valid-key? v) (string-trim-both v))
        ) ;let
        =>
        (lambda (v) v)
       ) ;
       (else
         (catch #t
           (lambda ()
             (let* ((env-dir (get-environment-variable "PI_CODING_AGENT_DIR"))
                    (agent-dir (if (pi-valid-key? env-dir)
                                 (pi-resolve-path env-dir (getcwd))
                                 (pi-resolve-path "~/.pi/agent" (getcwd))
                               ) ;if
                    ) ;agent-dir
                    (auth (pi-load-auth agent-dir))
                   ) ;
               (let ((k (pi-resolve-provider-key "google" auth)))
                 (if (pi-valid-key? k) (string-trim-both k) #f)
               ) ;let
             ) ;let*
           ) ;lambda
           (lambda _ #f)
         ) ;catch
       ) ;else
      ) ;cond
    ) ;define

    (define (pi-resolve-search-base-url config opt-url)
      (cond ((pi-valid-key? opt-url) (string-trim-both opt-url))
            ((let ((v (get-environment-variable "GOOGLE_BASE_URL")))
               (and (pi-valid-key? v) (string-trim-both v))
             ) ;let
             =>
             (lambda (v) v)
            ) ;
            ((let ((v (get-environment-variable "GEMINI_BASE_URL")))
               (and (pi-valid-key? v) (string-trim-both v))
             ) ;let
             =>
             (lambda (v) v)
            ) ;
            ((let ((v (get-environment-variable "GF_PI_BASE_URL")))
               (and (pi-valid-key? v) (string-trim-both v))
             ) ;let
             =>
             (lambda (v) v)
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
                 (string-trim-both (cdr bu))
                 "https://generativelanguage.googleapis.com/v1beta"
               ) ;if
             ) ;let*
            ) ;
            (else "https://generativelanguage.googleapis.com/v1beta")
      ) ;cond
    ) ;define

    (define (pi-resolve-search-proxy-url config opt-proxy)
      (cond ((pi-valid-key? opt-proxy) (string-trim-both opt-proxy))
            ((and (pair? config)
               (let ((pu (or (assoc 'proxy-url config) (assoc "proxy-url" config))))
                 (and (pair? pu) (pi-valid-key? (cdr pu)))
               ) ;let
             ) ;and
             (string-trim-both (cdr (or (assoc 'proxy-url config) (assoc "proxy-url" config)))
             ) ;string-trim-both
            ) ;
            ((let ((v (get-environment-variable "HTTPS_PROXY")))
               (and (pi-valid-key? v) (string-trim-both v))
             ) ;let
             =>
             (lambda (v) v)
            ) ;
            ((let ((v (get-environment-variable "ALL_PROXY")))
               (and (pi-valid-key? v) (string-trim-both v))
             ) ;let
             =>
             (lambda (v) v)
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
