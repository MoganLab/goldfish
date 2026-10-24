;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai kimi request)
  (import (scheme base)
    (scheme write)
    (liii string)
    (liii list)
    (liii uri)
    (liii json)
    (liii http)
    (liii error)
    (liii time)
    (liii pi ai event)
    (liii pi ai message)
    (liii pi ai kimi core)
    (liii pi ai kimi msg)
    (liii pi ai retry)
    (liii pi ai multikey)
  ) ;import
  (export kimi-build-tool-declarations-from-names kimi-request
    kimi-build-endpoint kimi-build-headers kimi-build-proxy-alist
  ) ;export
  (begin

    (define (%resp-field resp key)
      (cond ((procedure? resp) (resp key))
            ((hash-table? resp) (resp key))
            ((pair? resp) (cdr (or (assoc key resp) '(#f . #f))))
            (else #f)
      ) ;cond
    ) ;define

    (define (kimi-build-endpoint base-url)
      (let* ((u (make-uri (or base-url "https://api.kimi.com/coding/v1")))
             (raw-path (or (uri-path u) ""))
             (path
               (if (string-ends? raw-path "/")
                 (substring raw-path 0 (- (string-length raw-path) 1))
                 raw-path
               ) ;if
             ) ;path
            ) ;
        (cond ((string-ends? path "/chat/completions") (uri->string (uri-with-path u path)))
              ((or (string-ends? path "/v1")
                 (string-ends? path "/v4")
                 (string-contains? path "/paas/")
               ) ;or
               (uri->string (uri-join-path u "chat" "completions"))
              ) ;
              (else (uri->string (uri-join-path u "v1" "chat" "completions")))
        ) ;cond
      ) ;let*
    ) ;define

    (define (kimi-build-headers api-key)
      (list (cons "Content-Type" "application/json")
        (cons "Authorization" (string-append "Bearer " (or api-key "")))
        (cons "User-Agent" "Goldfish-Pi/1.0")
      ) ;list
    ) ;define

    (define (kimi-build-proxy-alist proxy-url)
      (if (and (string? proxy-url) (not (string-null? proxy-url)))
        (list (cons "http" proxy-url) (cons "https" proxy-url))
        '()
      ) ;if
    ) ;define

    (define (kimi-build-tool-declarations-from-names active-tool-names tool-resolver)
      (kimi-build-tool-declarations (filter (lambda (d) d) (map tool-resolver (or active-tool-names '())))
      ) ;kimi-build-tool-declarations
    ) ;define

    (define (%resolve-retry-params settings opt-retry)
      (pi-resolve-retry-params settings opt-retry)
    ) ;define

    (define (%make-retry-error status attempt body-str)
      (pi-assistant-message (list (pi-text-block ""))
        :stop-reason "error"
        :error-message
        (string-append "HTTP "
          (number->string status)
          " after "
          (number->string attempt)
          " attempts: "
          body-str
        ) ;string-append
        :usage (kimi-map-usage '())
      ) ;pi-assistant-message
    ) ;define

    (define (%post-kimi url req-json-str headers proxy-alist timeout)
      (catch #t
        (lambda ()
          (http-post url
            :data    req-json-str
            :headers headers
            :proxy   proxy-alist
            :timeout timeout
          ) ;http-post
        ) ;lambda
        (lambda (tag . args)
          (let ((err-msg (if (pair? args) (car args) "Network error")))
            (list (cons 'status-code 0) (cons 'reason err-msg) (cons 'text ""))
          ) ;let
        ) ;lambda
      ) ;catch
    ) ;define

    (define* (kimi-request config
               messages
               (system-instruction #f)
               (tool-names #f)
               (timeout 300)
               (opt-retry #f)
             ) ;kimi-request
      (let*-values (((model-id) (cdr (assoc 'model-id config)))
                    ((model-meta) (cdr (assoc 'model-meta config)))
                    ((proxy-url) (cdr (assoc 'proxy-url config)))
                    ((settings) (cdr (assoc 'settings config)))
                    ((max-retries base-delay max-delay provider-max-retries)
                     (%resolve-retry-params settings opt-retry)
                    ) ;
                    ((base-url)
                     (cdr (or (assoc 'base-url model-meta) '(#f
                                                             . "https://api.kimi.com/coding/v1")))
                    ) ;
                    ((max-tokens) (cdr (or (assoc 'max-tokens model-meta) '(#f
                                                                            . 8192))))
                    ((url) (kimi-build-endpoint base-url))
                    ((eff-tool-names) (or tool-names (cdr (assoc 'tools config))))
                    ((tool-resolver)
                     (if (assoc 'tool-resolver config)
                       (cdr (assoc 'tool-resolver config))
                       (lambda (name) #f)
                     ) ;if
                    ) ;
                    ((tool-decls)
                     (kimi-build-tool-declarations-from-names eff-tool-names tool-resolver)
                    ) ;
                    ((kimi-msgs) (kimi-build-messages messages system-instruction))
                    ((req-body)
                     (kimi-build-request-body model-id kimi-msgs #f tool-decls max-tokens)
                    ) ;
                    ((req-json-str) (json->string req-body))
                    ((proxy-alist) (kimi-build-proxy-alist proxy-url))
                   ) ;
        (let ((pool (pi-multikey-pool-of config)))
          (let loop
            ((attempt 1) (api-key (pi-multikey-request-key config)))
            (let* ((headers (kimi-build-headers api-key))
                   (resp (%post-kimi url req-json-str headers proxy-alist timeout))
                   (status (or (%resp-field resp 'status-code) 0))
                   (body-str (or (%resp-field resp 'text) ""))
                  ) ;
              (cond
               ((and pool (or (= status 429) (= status 403)))
                (let ((next (pi-multikey-rotate! config api-key status)))
                  (if (eq? next 'exhausted) (%make-retry-error status 1 body-str) (loop 1 next))
                ) ;let
               ) ;
               ((kimi-is-retriable-error? status body-str)
                (if (<= attempt provider-max-retries)
                  (let* ((ra-raw (pi-extract-retry-after resp))
                         (ra-sec (pi-parse-retry-after ra-raw))
                         (delay-sec (pi-calc-retry-delay attempt base-delay max-delay ra-sec))
                        ) ;
                    (pi-log-retry status attempt provider-max-retries delay-sec ra-sec)
                    (sleep delay-sec)
                    (loop (+ attempt 1) api-key)
                  ) ;let*
                  (%make-retry-error status attempt body-str)
                ) ;if
               ) ;
               (else
                 (let ((parsed-json (catch #t (lambda () (string->json body-str)) (lambda args #f))))
                   (kimi-parse-response parsed-json status)
                 ) ;let
               ) ;else
              ) ;cond
            ) ;let*
          ) ;let
        ) ;let
      ) ;let*-values
    ) ;define*

  ) ;begin
) ;define-library
