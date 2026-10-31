;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api openai-completions request)
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
    (liii pi ai api openai-completions core)
    (liii pi ai api openai-completions msg)
    (liii pi ai retry)
    (liii pi ai limits)
    (liii pi ai log)
    (liii pi ai keyring)
  ) ;import
  (export openai-build-tool-declarations-from-names openai-request
    openai-build-endpoint openai-build-headers openai-build-proxy-alist
  ) ;export
  (begin

    (define (%resp-field resp key)
      (cond ((procedure? resp) (resp key))
            ((hash-table? resp) (resp key))
            ((pair? resp) (cdr (or (assoc key resp) '(#f . #f))))
            (else #f)
      ) ;cond
    ) ;define

    (define (openai-build-endpoint base-url)
      (let* ((u (make-uri (or base-url "https://api.openai.com/v1")))
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

    (define (openai-build-headers api-key)
      (list (cons "Content-Type" "application/json")
        (cons "Authorization" (string-append "Bearer " (or api-key "")))
        (cons "User-Agent" "Goldfish-Pi/1.0")
      ) ;list
    ) ;define

    (define (openai-build-proxy-alist proxy-url)
      (if (and (string? proxy-url) (not (string-null? proxy-url)))
        (list (cons "http" proxy-url) (cons "https" proxy-url))
        '()
      ) ;if
    ) ;define

    (define (openai-build-tool-declarations-from-names active-tool-names tool-resolver)
      (openai-build-tool-declarations (filter (lambda (d) d) (map tool-resolver (or active-tool-names '())))
      ) ;openai-build-tool-declarations
    ) ;define

    (define (%resolve-retry-params settings opt-retry)
      (pi-resolve-retry-params settings opt-retry)
    ) ;define

    (define (%make-retry-error status attempt body-str provider)
      (let ((limit (pi-usage-limit-error? status body-str)))
        (pi-assistant-message (list (pi-text-block ""))
          :stop-reason "error"
          :error-message
          (if limit
            (pi-usage-limit-error-message limit provider)
            (string-append "HTTP "
              (number->string status)
              " after "
              (number->string attempt)
              " attempts: "
              body-str
            ) ;string-append
          ) ;if
          :usage (openai-map-usage '())
        ) ;pi-assistant-message
      ) ;let
    ) ;define

    (define (%post-openai url req-json-str headers proxy-alist timeout)
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

    (define* (openai-request config
               messages
               (system-instruction #f)
               (tool-names #f)
               (timeout 300)
               (opt-retry #f)
             ) ;openai-request
      (let*-values (((model-id) (cdr (assoc 'model-id config)))
                    ((model-meta) (cdr (assoc 'model-meta config)))
                    ((proxy-url) (cdr (assoc 'proxy-url config)))
                    ((settings) (cdr (assoc 'settings config)))
                    ((provider) (cdr (or (assoc 'provider config) '(#f . #f))))
                    ((max-retries base-delay max-delay provider-max-retries)
                     (%resolve-retry-params settings opt-retry)
                    ) ;
                    ((base-url)
                     (cdr (or (assoc 'base-url model-meta) '(#f
                                                             . "https://api.openai.com/v1")))
                    ) ;
                    ((max-tokens) (cdr (or (assoc 'max-tokens model-meta) '(#f
                                                                            . 8192))))
                    ((url) (openai-build-endpoint base-url))
                    ((eff-tool-names) (or tool-names (cdr (assoc 'tools config))))
                    ((tool-resolver)
                     (if (assoc 'tool-resolver config)
                       (cdr (assoc 'tool-resolver config))
                       (lambda (name) #f)
                     ) ;if
                    ) ;
                    ((tool-decls)
                     (openai-build-tool-declarations-from-names eff-tool-names tool-resolver)
                    ) ;
                    ((openai-msgs) (openai-build-messages messages system-instruction))
                    ((req-body)
                     (openai-build-request-body model-id openai-msgs #f tool-decls max-tokens)
                    ) ;
                    ((req-json-str) (json->string req-body))
                    ((proxy-alist) (openai-build-proxy-alist proxy-url))
                   ) ;
        (let ((key-rotation? (pi-key-rotation? config)))
          (let loop
            ((attempt 1) (api-key (pi-request-api-key config)))
            (let* ((headers (openai-build-headers api-key))
                   (resp (%post-openai url req-json-str headers proxy-alist timeout))
                   (status (or (%resp-field resp 'status-code) 0))
                   (body-str (or (%resp-field resp 'text) ""))
                  ) ;
              ;; 异常请求（非 200）连同响应体原文落盘，便于事后排障
              (when (not (= status 200))
                (pi-log-http-error! status
                  body-str
                  (list (cons 'provider provider)
                    (cons 'model model-id)
                    (cons 'url (string-append "POST " url))
                    (cons 'attempt attempt)
                    (cons 'max-attempts provider-max-retries)
                    (cons 'retryable (openai-is-retriable-error? status body-str))
                  ) ;list
                ) ;pi-log-http-error!
              ) ;when
              (cond
               ((and key-rotation? (or (= status 429) (= status 403)))
                (let ((next (pi-rotate-api-key! config api-key status)))
                  (if (eq? next 'exhausted)
                    (%make-retry-error status 1 body-str provider)
                    (loop 1 next)
                  ) ;if
                ) ;let
               ) ;
               ((openai-is-retriable-error? status body-str)
                (if (<= attempt provider-max-retries)
                  (let* ((ra-raw (pi-extract-retry-after resp))
                         (ra-sec (pi-parse-retry-after ra-raw))
                         (delay-sec (pi-calc-retry-delay attempt base-delay max-delay ra-sec))
                        ) ;
                    (pi-log-retry status attempt provider-max-retries delay-sec ra-sec)
                    (sleep delay-sec)
                    (loop (+ attempt 1) api-key)
                  ) ;let*
                  (%make-retry-error status attempt body-str provider)
                ) ;if
               ) ;
               (else
                 (let* ((parsed-json (catch #t (lambda () (string->json body-str)) (lambda args #f)))
                        (limit (pi-usage-limit-error? status body-str))
                       ) ;
                   (if limit
                     (pi-assistant-message (list (pi-text-block ""))
                       :stop-reason "error"
                       :usage       (openai-map-usage '())
                       :error-message
                       (pi-usage-limit-error-message limit provider)
                     ) ;pi-assistant-message
                     (openai-parse-response parsed-json status)
                   ) ;if
                 ) ;let*
               ) ;else
              ) ;cond
            ) ;let*
          ) ;let
        ) ;let
      ) ;let*-values
    ) ;define*

  ) ;begin
) ;define-library
