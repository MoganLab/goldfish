;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai anthropic request)
  (import (scheme base)
    (scheme write)
    (liii string)
    (liii list)
    (liii json)
    (liii http)
    (liii error)
    (liii time)
    (liii pi ai event)
    (liii pi ai message)
    (liii pi ai anthropic core)
    (liii pi ai anthropic thinking)
    (liii pi ai retry)
  ) ;import
  (export anthropic-build-tool-declarations-from-names anthropic-request)
  (begin

    (define (%resp-field resp key)
      (cond ((procedure? resp) (resp key))
            ((hash-table? resp) (resp key))
            ((pair? resp) (cdr (or (assoc key resp) '(#f . #f))))
            (else #f)
      ) ;cond
    ) ;define

    (define (anthropic-build-tool-declarations-from-names active-tool-names tool-resolver)
      (anthropic-build-tool-declarations (filter (lambda (d) d) (map tool-resolver (or active-tool-names '())))
      ) ;anthropic-build-tool-declarations
    ) ;define

    (define (%build-anthropic-headers api-key)
      (let ((base-headers (list (cons "Content-Type" "application/json")
                            (cons "anthropic-version" "2023-06-01")
                          ) ;list
            ) ;base-headers
           ) ;
        (if (and (string? api-key) (string-starts? api-key "Bearer "))
          (cons (cons "Authorization" api-key) base-headers)
          (cons (cons "x-api-key" (or api-key "")) base-headers)
        ) ;if
      ) ;let
    ) ;define

    (define (%build-proxy-alist proxy-url)
      (if (and (string? proxy-url) (not (string-null? proxy-url)))
        (list (cons "http" proxy-url) (cons "https" proxy-url))
        '()
      ) ;if
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
        :usage (anthropic-map-usage '())
      ) ;pi-assistant-message
    ) ;define

    (define (%post-anthropic url req-json-str headers proxy-alist timeout)
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

    (define* (anthropic-request config
               messages
               (system-instruction #f)
               (tool-names #f)
               (timeout 300)
               (opt-retry #f)
             ) ;anthropic-request
      (let*-values (((model-id) (cdr (assoc 'model-id config)))
                    ((model-meta) (cdr (assoc 'model-meta config)))
                    ((api-key) (cdr (assoc 'api-key config)))
                    ((proxy-url) (cdr (assoc 'proxy-url config)))
                    ((settings) (cdr (assoc 'settings config)))
                    ((max-retries base-delay max-delay provider-max-retries)
                     (%resolve-retry-params settings opt-retry)
                    ) ;
                    ((base-url)
                     (cdr (or (assoc 'base-url model-meta) '(#f
                                                             . "https://api.anthropic.com")))
                    ) ;
                    ((max-tokens) (cdr (or (assoc 'max-tokens model-meta) '(#f
                                                                            . 8192))))
                    ((url) (string-append base-url "/v1/messages"))
                    ((eff-tool-names) (or tool-names (cdr (assoc 'tools config))))
                    ((tool-resolver)
                     (if (assoc 'tool-resolver config)
                       (cdr (assoc 'tool-resolver config))
                       (lambda (name) #f)
                     ) ;if
                    ) ;
                    ((tool-decls)
                     (anthropic-build-tool-declarations-from-names eff-tool-names tool-resolver)
                    ) ;
                    ((ant-msgs) (anthropic-build-messages messages))
                    ((raw-body)
                     (anthropic-build-request-body model-id ant-msgs
                       system-instruction tool-decls max-tokens
                     ) ;anthropic-build-request-body
                    ) ;
                    ((thinking-lvl) (cdr (or (assoc 'thinking-level config) '(#f
                                                                              . #f))))
                    ((req-body) (anthropic-inject-thinking raw-body thinking-lvl))
                    ((req-json-str) (json->string req-body))
                    ((headers) (%build-anthropic-headers api-key))
                    ((proxy-alist) (%build-proxy-alist proxy-url))
                   ) ;
        (let loop
          ((attempt 1))
          (let* ((resp (%post-anthropic url req-json-str headers proxy-alist timeout))
                 (status (or (%resp-field resp 'status-code) 0))
                 (body-str (or (%resp-field resp 'text) ""))
                ) ;
            (if (anthropic-is-retriable-error? status body-str)
              (if (<= attempt provider-max-retries)
                (let* ((ra-raw (pi-extract-retry-after resp))
                       (ra-sec (pi-parse-retry-after ra-raw))
                       (delay-sec (pi-calc-retry-delay attempt base-delay max-delay ra-sec))
                      ) ;
                  (pi-log-retry status attempt provider-max-retries delay-sec ra-sec)
                  (sleep delay-sec)
                  (loop (+ attempt 1))
                ) ;let*
                (%make-retry-error status attempt body-str)
              ) ;if
              (let ((parsed-json (catch #t (lambda () (string->json body-str)) (lambda args #f))))
                (anthropic-parse-response parsed-json status)
              ) ;let
            ) ;if
          ) ;let*
        ) ;let
      ) ;let*-values
    ) ;define*

  ) ;begin
) ;define-library
