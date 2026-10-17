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

(define-library (liii goldpi-anthropic-stream)
  (import (scheme base)
    (scheme write)
    (liii string)
    (liii json)
    (liii http)
    (liii time)
    (liii pi)
    (liii goldpi-anthropic)
    (liii goldpi-anthropic-parse)
  ) ;import
  (export anthropic-request-stream)
  (begin

    (define (%resp-field resp key)
      (cond ((procedure? resp) (resp key))
            ((hash-table? resp) (resp key))
            ((pair? resp) (cdr (or (assoc key resp) '(#f . #f))))
            (else #f)
      ) ;cond
    ) ;define

    (define (%safe-assoc key alist)
      (if (and (pair? alist) (every pair? alist)) (assoc key alist) #f)
    ) ;define

    (define (%build-req-body model-id msgs system-prompt tool-decls max-tokens)
      (append (anthropic-build-request-body model-id msgs system-prompt tool-decls max-tokens)
        (list (cons "stream" #t))
      ) ;append
    ) ;define

    (define (%build-headers api-key)
      (list (cons "Content-Type" "application/json")
        (cons "x-api-key" (or api-key ""))
        (cons "anthropic-version" "2023-06-01")
      ) ;list
    ) ;define

    (define (%build-proxy proxy-url)
      (if (and (string? proxy-url) (not (string-null? proxy-url)))
        (list (cons "http" proxy-url) (cons "https" proxy-url))
        '()
      ) ;if
    ) ;define

    (define (%make-stream-cb stream-state on-event effective-idle on-idle)
      (let* ((last-act (time-second (current-time)))
             (decoder (make-anthropic-stream-decoder stream-state on-event))
            ) ;
        (lambda (chunk)
          (let ((now (time-second (current-time))))
            (if (and effective-idle (> (- now last-act) effective-idle))
              (begin
                (on-idle)
                #f
              ) ;begin
              (begin
                (set! last-act now)
                (decoder chunk)
                #t
              ) ;begin
            ) ;if
          ) ;let
        ) ;lambda
      ) ;let*
    ) ;define

    (define (%finalize-stream stream-state)
      (let* ((blocks (vector-ref stream-state 0))
             (raw-stop (vector-ref stream-state 4))
             (stop-reason (anthropic-map-stop-reason (or raw-stop "end_turn")))
             (err-msg (if (equal? stop-reason "error") raw-stop #f))
            ) ;
        (list (cons 'role "assistant")
          (cons 'content blocks)
          (cons 'stopReason stop-reason)
          (cons 'usage (vector-ref stream-state 2))
          (cons 'errorMessage err-msg)
        ) ;list
      ) ;let*
    ) ;define

    (define (%make-idle-timeout-error eff-idle)
      (list (cons 'role "assistant")
        (cons 'content '())
        (cons 'stopReason "error")
        (cons 'usage (anthropic-map-usage '()))
        (cons 'errorMessage
          (string-append "Anthropic stream idle timeout after "
            (number->string eff-idle)
            " seconds"
          ) ;string-append
        ) ;cons
      ) ;list
    ) ;define

    (define (%make-stream-error status)
      (list (cons 'role "assistant")
        (cons 'content '())
        (cons 'stopReason "error")
        (cons 'usage (anthropic-map-usage '()))
        (cons 'errorMessage
          (string-append "HTTP " (number->string status) " stream error")
        ) ;cons
      ) ;list
    ) ;define

    (define (%post-stream url req-json-str headers proxy-alist cb timeout eff-idle)
      (catch #t
        (lambda ()
          (http-post url
            :data         req-json-str
            :headers      headers
            :proxy        proxy-alist
            :stream       #t
            :callback     cb
            :timeout      timeout
            :idle-timeout eff-idle
          ) ;http-post
        ) ;lambda
        (lambda (tag . args)
          (let ((err-msg (if (pair? args) (car args) "Network error")))
            (list (cons 'status-code 0) (cons 'reason err-msg) (cons 'text ""))
          ) ;let
        ) ;lambda
      ) ;catch
    ) ;define

    (define (%init-anthropic-stream-state)
      (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f)
    ) ;define

    (define (%run-stream-loop url req-json-str headers proxy-alist on-ev timeout
              opt-retry eff-idle
            ) ;%run-stream-loop
      (let ((max-r (cdr (or (%safe-assoc 'max-retries (or opt-retry '())) '(#f
                                                                            . 3)))))
        (let loop
          ((attempt 1))
          (let* ((stream-state (%init-anthropic-stream-state))
                 (idle-timed-out? #f)
                 (cb (%make-stream-cb stream-state
                       on-ev
                       eff-idle
                       (lambda () (set! idle-timed-out? #t))
                     ) ;%make-stream-cb
                 ) ;cb
                 (resp (%post-stream url req-json-str headers proxy-alist cb timeout eff-idle))
                 (status (or (%resp-field resp 'status-code) 0))
                ) ;
            (cond (idle-timed-out? (%make-idle-timeout-error eff-idle))
                  ((anthropic-is-retriable-error? status "")
                   (if (<= attempt max-r)
                     (begin
                       (sleep 1.0)
                       (loop (+ attempt 1))
                     ) ;begin
                     (%make-stream-error status)
                   ) ;if
                  ) ;
                  (else (%finalize-stream stream-state))
            ) ;cond
          ) ;let*
        ) ;let
      ) ;let
    ) ;define

    (define* (anthropic-request-stream config
               messages
               (system-instruction #f)
               (tool-names #f)
               (on-event #f)
               (timeout 300)
               (opt-retry #f)
               (idle-timeout #f)
             ) ;anthropic-request-stream
      (let* ((model-id (cdr (assoc 'model-id config)))
             (model-meta (cdr (assoc 'model-meta config)))
             (api-key (cdr (assoc 'api-key config)))
             (proxy-url (cdr (assoc 'proxy-url config)))
             (base-url (cdr (or (assoc 'base-url model-meta) '(#f
                                                               . "https://api.anthropic.com")))
             ) ;base-url
             (max-tokens (cdr (or (assoc 'max-tokens model-meta) '(#f . 8192))))
             (url (string-append base-url "/v1/messages"))
             (eff-tool-names (or tool-names (cdr (assoc 'tools config))))
             (tool-decls (anthropic-build-tool-declarations-from-names eff-tool-names))
             (ant-msgs (anthropic-build-messages messages))
             (req-body (%build-req-body model-id ant-msgs system-instruction tool-decls max-tokens)
             ) ;req-body
             (req-json-str (json->string req-body))
             (headers (%build-headers api-key))
             (proxy-alist (%build-proxy proxy-url))
             (eff-idle (or idle-timeout timeout))
            ) ;
        (%run-stream-loop url req-json-str headers proxy-alist on-event timeout
          opt-retry eff-idle
        ) ;%run-stream-loop
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
