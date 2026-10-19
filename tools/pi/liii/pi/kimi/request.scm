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

(define-library (liii pi kimi request)
  (import (scheme base)
    (scheme write)
    (liii string)
    (liii json)
    (liii http)
    (liii error)
    (liii time)
    (liii pi event)
    (liii pi message)
    (liii pi kimi core)
    (liii pi kimi msg)
    (liii pi tools)
    (liii pi retry)
    (liii pi multikey)
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
      (let* ((raw (or base-url "https://api.kimi.com/coding/v1"))
             (b
               (if (string-ends? raw "/") (substring raw 0 (- (string-length raw) 1)) raw)
             ) ;b
            ) ;
        (cond ((string-ends? b "/chat/completions") b)
              ((string-ends? b "/v1") (string-append b "/chat/completions"))
              (else (string-append b "/v1/chat/completions"))
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

    (define (kimi-build-tool-declarations-from-names active-tool-names)
      (let loop
        ((names (or active-tool-names '())) (acc '()))
        (if (null? names)
          (kimi-build-tool-declarations (reverse acc))
          (let* ((name (car names)) (def (get-tool-definition name)))
            (if def (loop (cdr names) (cons def acc)) (loop (cdr names) acc))
          ) ;let*
        ) ;if
      ) ;let
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
                    ((tool-decls) (kimi-build-tool-declarations-from-names eff-tool-names))
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
                  (begin
                    (sleep (%calc-delay-sec base-delay max-delay attempt))
                    (loop (+ attempt 1) api-key)
                  ) ;begin
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
