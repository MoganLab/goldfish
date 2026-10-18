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

(define-library (liii goldpi-stream)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii json)
    (liii http)
    (liii time)
    (liii pi)
    (liii goldpi-tools)
    (liii goldpi-provider)
    (liii goldpi-stream-parse)
  ) ;import
  (export gemini-request-stream)
  (begin

    (define (%safe-assoc key alist)
      (if (and (pair? alist) (every pair? alist)) (assoc key alist) #f)
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
      (string-append (cdr (or (assoc 'base-url model-meta)
                            '(#f
                              . "https://generativelanguage.googleapis.com/v1beta")
                          ) ;or
                     ) ;cdr
        "/models/"
        model-id
        ":streamGenerateContent?alt=sse"
      ) ;string-append
    ) ;define

    (define (%build-stream-headers api-key)
      (list (cons "Content-Type" "application/json")
        (cons "x-goog-api-key" (or api-key ""))
      ) ;list
    ) ;define

    (define (%build-stream-proxy proxy-url)
      (if (and (string? proxy-url) (not (string-null? proxy-url)))
        (list (cons "http" proxy-url) (cons "https" proxy-url))
        '()
      ) ;if
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

    (define (%finalize-stream-success stream-state on-event)
      (let* ((blocks (vector-ref stream-state 0))
             (has-fn? (pair? (vector-ref stream-state 3)))
             (raw-finish (vector-ref stream-state 4))
             (finish-reason (or raw-finish "STOP"))
             (stop-reason (gemini-map-finish-reason finish-reason has-fn?))
             (err-msg (if (equal? stop-reason "error") finish-reason #f))
            ) ;
        (when (and (string? (vector-ref stream-state 1)) (procedure? on-event))
          (on-event
            (list (cons 'type "text_end")
              (cons 'contentIndex (- (length blocks) 1))
              (cons 'content (vector-ref stream-state 1))
              (cons 'usage (vector-ref stream-state 2))
            ) ;list
          ) ;on-event
        ) ;when
        (list (cons 'role "assistant")
          (cons 'content blocks)
          (cons 'stopReason stop-reason)
          (cons 'usage (vector-ref stream-state 2))
          (cons 'errorMessage err-msg)
        ) ;list
      ) ;let*
    ) ;define

    (define (%classify-stream-result stream-state idle-timed-out? resp status
              eff-idle on-event error-body-acc
            ) ;%classify-stream-result
      (let ((resp-err (or (%resp-field resp 'error) (%resp-field resp 'reason) "")))
        (cond
         ((or idle-timed-out?
            (string-contains? (string-downcase resp-err) "too slow")
            (string-contains? (string-downcase resp-err) "low speed")
            (string-contains? (string-downcase resp-err) "timed out")
          ) ;or
          (list (cons 'role "assistant")
            (cons 'content (vector-ref stream-state 0))
            (cons 'stopReason "error")
            (cons 'usage (vector-ref stream-state 2))
            (cons 'errorMessage
              (string-append "Stream idle timed out after "
                (number->string eff-idle)
                " seconds"
              ) ;string-append
            ) ;cons
          ) ;list
         ) ;
         ((and (not (zero? status)) (not (= status 200)))
          (let ((body
                  (if (not (string-null? (vector-ref error-body-acc 0)))
                    (vector-ref error-body-acc 0)
                    (or (%resp-field resp 'body) (%resp-field resp 'text) "")
                  ) ;if
                ) ;body
               ) ;
            (list (cons 'role "assistant")
              (cons 'content (vector-ref stream-state 0))
              (cons 'stopReason "error")
              (cons 'usage (vector-ref stream-state 2))
              (cons 'errorMessage
                (let ((parsed-msg (extract-stream-error-message body)))
                  (if (and parsed-msg (not (string=? parsed-msg "")))
                    (string-append "HTTP " (number->string status) ": " parsed-msg)
                    (string-append "HTTP " (number->string status) " error")
                  ) ;if
                ) ;let
              ) ;cons
            ) ;list
          ) ;let
         ) ;
         (else (%finalize-stream-success stream-state on-event))
        ) ;cond
      ) ;let
    ) ;define

    (define (%calc-retry-delay attempt opt-retry)
      (let* ((base-ms
               (cdr (or (%safe-assoc 'base-delay-ms (or opt-retry '())) '(#f
                                                                          . 2000)))
             ) ;base-ms
             (max-ms
               (cdr (or (%safe-assoc 'max-agent-delay-ms (or opt-retry '())) '(#f
                                                                               . 60000)))
             ) ;max-ms
             (calc-delay (* base-ms (expt 2 (- attempt 1))))
            ) ;
        (/ (min calc-delay max-ms) 1000.0)
      ) ;let*
    ) ;define

    (define (%build-stream-req-params config tool-names messages sys-inst)
      (let* ((api-key (cdr (assoc 'api-key config)))
             (model-id (cdr (assoc 'model-id config)))
             (model-meta (cdr (assoc 'model-meta config)))
             (proxy-url (cdr (assoc 'proxy-url config)))
             (eff-tool-names (or tool-names (cdr (assoc 'tools config))))
             (tool-decls (gemini-build-tool-declarations eff-tool-names))
             (contents (gemini-build-contents messages))
             (req-body (gemini-build-request-body contents sys-inst tool-decls))
            ) ;
        (values (%build-stream-url model-meta model-id)
          (json->string req-body)
          (%build-stream-headers api-key)
          (%build-stream-proxy proxy-url)
        ) ;values
      ) ;let*
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
      (let-values (((url req-json-str headers proxy-alist)
                    (%build-stream-req-params config tool-names messages system-instruction)
                   ) ;
                  ) ;
        (let* ((effective-idle (or idle-timeout timeout))
               (max-retries
                 (if
                   (or (not opt-retry) (cdr (or (%safe-assoc 'enabled opt-retry) '(#f
                                                                                   . #t))))
                   (cdr (or (%safe-assoc 'max-retries (or opt-retry '())) '(#f
                                                                            . 3)))
                   0
                 ) ;if
               ) ;max-retries
              ) ;
          (let loop
            ((attempt 1))
            (let* ((stream-state (vector '() #f (gemini-map-usage '()) '() #f))
                   (idle-timed-out? #f)
                   (error-body-acc (vector ""))
                   (stream-cb (%make-stream-callback stream-state
                                on-event
                                effective-idle
                                (lambda () (set! idle-timed-out? #t))
                                error-body-acc
                              ) ;%make-stream-callback
                   ) ;stream-cb
                   (resp (%perform-http-stream url req-json-str headers
                           proxy-alist stream-cb timeout effective-idle
                         ) ;%perform-http-stream
                   ) ;resp
                   (status (or (%resp-field resp 'status-code) 0))
                  ) ;
              (if (and (not idle-timed-out?) (gemini-is-retriable-error? status ""))
                (if (<= attempt max-retries)
                  (begin
                    (sleep (%calc-retry-delay attempt opt-retry))
                    (loop (+ attempt 1))
                  ) ;begin
                  (list (cons 'role "assistant")
                    (cons 'content (vector-ref stream-state 0))
                    (cons 'stopReason "error")
                    (cons 'usage (vector-ref stream-state 2))
                    (cons 'errorMessage
                      (string-append "HTTP " (number->string status) " stream error")
                    ) ;cons
                  ) ;list
                ) ;if
                (%classify-stream-result stream-state idle-timed-out? resp
                  status effective-idle on-event error-body-acc
                ) ;%classify-stream-result
              ) ;if
            ) ;let*
          ) ;let
        ) ;let*
      ) ;let-values
    ) ;define*

  ) ;begin
) ;define-library
