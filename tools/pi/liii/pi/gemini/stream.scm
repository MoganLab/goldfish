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

(define-library (liii pi gemini stream)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii json)
    (liii http)
    (liii time)
    (liii pi tools)
    (liii pi event)
    (liii pi config types)
    (liii pi retry)
    (liii pi message)
    (liii pi multikey)
    (liii pi gemini core)
    (liii pi gemini request)
    (liii pi gemini stream-parse)
    (liii pi gemini stream-result)
    (liii pi gemini thinking)
  ) ;import
  (export gemini-request-stream)
  (begin

    (define (%safe-assoc key alist)
      (cond ((or (pi-settings? alist) (pi-retry-config? alist) (pi-model-metadata? alist))
             (assoc key alist)
            ) ;
            ((and (pair? alist) (every pair? alist)) (assoc key alist))
            (else #f)
      ) ;cond
    ) ;define

    (define (%resp-field resp key)
      (cond ((procedure? resp) (resp key))
            ((hash-table? resp) (resp key))
            ((pair? resp) (cdr (or (assoc key resp) '(#f . #f))))
            (else #f)
      ) ;cond
    ) ;define

    (define (%make-stream-callback stream-state on-event effective-idle
              on-idle-timeout error-body-acc
            ) ;%make-stream-callback
      (let* ((last-activity-sec (time-second (current-time)))
             (sse-decoder (make-stream-decoder stream-state on-event))
            ) ;
        (lambda (chunk)
          (vector-set! error-body-acc
            0
            (string-append (vector-ref error-body-acc 0) chunk)
          ) ;vector-set!
          (let ((now (time-second (current-time))))
            (if (and effective-idle (> (- now last-activity-sec) effective-idle))
              (begin
                (on-idle-timeout)
                #f
              ) ;begin
              (begin
                (set! last-activity-sec now)
                (sse-decoder chunk)
                #t
              ) ;begin
            ) ;if
          ) ;let
        ) ;lambda
      ) ;let*
    ) ;define

    (define (%build-stream-url model-meta model-id)
      (let ((base (or (%safe-assoc 'base-url model-meta)
                    '(#f . "https://generativelanguage.googleapis.com/v1beta")
                  ) ;or
            ) ;base
           ) ;
        (string-append (cdr base) "/models/" model-id ":streamGenerateContent?alt=sse")
      ) ;let
    ) ;define

    (define (%build-stream-req-params config tool-names messages sys-inst)
      (let* ((model-id (cdr (assoc 'model-id config)))
             (model-meta (cdr (assoc 'model-meta config)))
             (proxy-url (cdr (assoc 'proxy-url config)))
             (eff-tool-names (or tool-names (cdr (assoc 'tools config))))
             (tool-decls (gemini-build-tool-declarations eff-tool-names))
             (contents (gemini-build-contents messages))
             (raw-req-body (gemini-build-request-body contents sys-inst tool-decls))
             (thinking-lvl (cdr (or (assoc 'thinking-level config) '(#f . #f))))
             (req-body (gemini-inject-thinking-config raw-req-body thinking-lvl))
             (proxy-alist
               (if (and (string? proxy-url) (not (string-null? proxy-url)))
                 (list (cons "http" proxy-url) (cons "https" proxy-url))
                 '()
               ) ;if
             ) ;proxy-alist
            ) ;
        (values (%build-stream-url model-meta model-id)
          (json->string req-body)
          proxy-alist
        ) ;values
      ) ;let*
    ) ;define

    (define (%perform-http-stream url req-json-str headers proxy-alist stream-cb
              timeout eff-idle
            ) ;%perform-http-stream
      (catch #t
        (lambda ()
          (http-post url
            :data         req-json-str
            :headers      headers
            :proxy        proxy-alist
            :stream       #t
            :callback     stream-cb
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

    (define* (gemini-request-stream config
               messages
               (system-instruction #f)
               (tool-names #f)
               (on-event #f)
               (timeout 300)
               (opt-retry #f)
               (idle-timeout #f)
             ) ;gemini-request-stream
      (let-values (((url req-json-str proxy-alist)
                    (%build-stream-req-params config tool-names messages system-instruction)
                   ) ;
                  ) ;
        (let* ((settings (cdr (or (%safe-assoc 'settings config) '(#f))))
               (effective-idle (or idle-timeout timeout))
               (pool (pi-multikey-pool-of config))
              ) ;
          (let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
                        (pi-resolve-retry-params settings opt-retry)
                       ) ;
                      ) ;
            (let loop
              ((attempt 1) (api-key (pi-multikey-request-key config)))
              (let* ((eff-headers (list (cons "Content-Type" "application/json")
                                    (cons "x-goog-api-key" (or api-key ""))
                                  ) ;list
                     ) ;eff-headers
                     (stream-state (vector '() #f (gemini-map-usage '()) '() #f #f #f #f))
                     (idle-timed-out? #f)
                     (emitted? #f)
                     (wrapped-on-event (if (procedure? on-event) (lambda (ev) (set! emitted? #t) (on-event ev)) #f)
                     ) ;wrapped-on-event
                     (error-body-acc (vector ""))
                     (stream-cb (%make-stream-callback stream-state
                                  wrapped-on-event
                                  effective-idle
                                  (lambda () (set! idle-timed-out? #t))
                                  error-body-acc
                                ) ;%make-stream-callback
                     ) ;stream-cb
                     (resp (%perform-http-stream url req-json-str eff-headers
                             proxy-alist stream-cb timeout effective-idle
                           ) ;%perform-http-stream
                     ) ;resp
                     (status (or (%resp-field resp 'status-code) 0))
                    ) ;
                (cond
                 ((and pool (not emitted?) (or (= status 429) (= status 403)))
                  (let ((next (pi-multikey-rotate! config api-key status)))
                    (if (eq? next 'exhausted)
                      (gemini-classify-stream-result stream-state
                        idle-timed-out? resp status effective-idle
                        wrapped-on-event error-body-acc
                      ) ;gemini-classify-stream-result
                      (loop 1 next)
                    ) ;if
                  ) ;let
                 ) ;
                 ((and (not idle-timed-out?) (gemini-is-retriable-error? status ""))
                  (if (<= attempt provider-max-retries)
                    (begin
                      (sleep (pi-calc-retry-delay attempt 2000 60000))
                      (loop (+ attempt 1) api-key)
                    ) ;begin
                    (list (cons 'role "assistant")
                      (cons 'content (vector-ref stream-state 0))
                      (cons 'stopReason "error")
                      (cons 'usage (vector-ref stream-state 2))
                      (cons 'errorMessage (format-failed-stream-msg status resp))
                    ) ;list
                  ) ;if
                 ) ;
                 (else (gemini-classify-stream-result stream-state
                         idle-timed-out? resp status effective-idle
                         wrapped-on-event error-body-acc
                       ) ;gemini-classify-stream-result
                 ) ;else
                ) ;cond
              ) ;let*
            ) ;let
          ) ;let-values
        ) ;let*
      ) ;let-values
    ) ;define*

  ) ;begin
) ;define-library
