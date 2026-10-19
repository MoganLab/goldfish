;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii pi anthropic request)
  (import (scheme base)
    (scheme write)
    (liii string)
    (liii json)
    (liii http)
    (liii error)
    (liii time)
    (liii pi event)
    (liii pi message)
    (liii pi anthropic core)
    (liii pi anthropic thinking)
    (liii pi tools)
    (liii pi retry)
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

    (define (anthropic-build-tool-declarations-from-names active-tool-names)
      (let loop
        ((names active-tool-names) (acc '()))
        (if (null? names)
          (anthropic-build-tool-declarations (reverse acc))
          (let* ((name (car names)) (def (get-tool-definition name)))
            (if def (loop (cdr names) (cons def acc)) (loop (cdr names) acc))
          ) ;let*
        ) ;if
      ) ;let
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

    (define (%calc-delay-sec base-delay max-delay attempt)
      (let* ((calc-ms (* base-delay (expt 2 (- attempt 1))))
             (delay-ms (min calc-ms max-delay))
            ) ;
        (/ delay-ms 1000.0)
      ) ;let*
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
                    ((tool-decls) (anthropic-build-tool-declarations-from-names eff-tool-names))
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
                (begin
                  (sleep (%calc-delay-sec base-delay max-delay attempt))
                  (loop (+ attempt 1))
                ) ;begin
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
