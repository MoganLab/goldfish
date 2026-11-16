;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api google-generative-ai stream)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii json)
    (liii http)
    (liii time)
    (liii pi ai abort)
    (liii pi ai time)
    (liii pi ai event)
    (liii pi ai config types)
    (liii pi ai config runtime)
    (liii pi ai retry)
    (liii pi ai limits)
    (liii pi ai log)
    (liii pi ai message)
    (liii pi ai keyring)
    (liii pi ai api google-generative-ai core)
    (liii pi ai api google-generative-ai usage)
    (liii pi ai api google-generative-ai request)
    (liii pi ai api google-generative-ai stream-parse)
    (liii pi ai api google-generative-ai stream-state)
    (liii pi ai api google-generative-ai stream-result)
    (liii pi ai api google-generative-ai thinking)
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
              on-idle-timeout on-abort error-body-acc last-act
            ) ;%make-stream-callback
      (let ((sse-decoder (make-stream-decoder stream-state on-event)))
        (lambda (chunk)
          (if (pi-aborted?)
            (begin
              (on-abort)
              #f
            ) ;begin
            (let ((now (pi-current-monotonic-seconds)))
              (vector-set! error-body-acc
                0
                (string-append (vector-ref error-body-acc 0) chunk)
              ) ;vector-set!
              (if
                (and effective-idle (> (- now (vector-ref last-act 0)) effective-idle))
                (begin
                  (on-idle-timeout)
                  #f
                ) ;begin
                (begin
                  (vector-set! last-act 0 now)
                  (sse-decoder chunk)
                  #t
                ) ;begin
              ) ;if
            ) ;let
          ) ;if
        ) ;lambda
      ) ;let
    ) ;define

    (define (%finalize-aborted-stream stream-state provider model-id)
      (pi-assistant-message (gemini-stream-state-blocks stream-state)
        :stop-reason   "aborted"
        :error-message #f
        :usage         (gemini-stream-state-usage stream-state)
        :provider      provider
        :model         model-id
        :api           "google-generative-ai"
      ) ;pi-assistant-message
    ) ;define

    (define (%build-stream-req-params config tool-names messages sys-inst)
      (let* ((model-id (pi-cfg-ref config 'model-id))
             (model-meta (pi-cfg-ref config 'model-meta))
             (proxy-url (pi-cfg-ref config 'proxy-url))
             (provider (pi-cfg-ref config 'provider "google"))
             (eff-tool-names (or tool-names (pi-cfg-ref config 'tools '())))
             (tool-resolver (or (pi-cfg-ref config 'tool-resolver) (lambda (name) #f)))
             (tool-decls (gemini-build-tool-declarations eff-tool-names tool-resolver))
             (contents (gemini-build-contents messages model-id (or provider "google")))
             (raw-req-body (gemini-build-request-body contents sys-inst tool-decls))
             (thinking-lvl (pi-cfg-ref config 'thinking-level))
             (req-body (gemini-inject-thinking-config raw-req-body thinking-lvl))
             (proxy-alist
               (if (and (string? proxy-url) (not (string-null? proxy-url)))
                 `(("http" . ,proxy-url) ("https" . ,proxy-url))
                 '()
               ) ;if
             ) ;proxy-alist
             (url
               (gemini-build-stream-endpoint (cdr (or (%safe-assoc 'base-url model-meta) '(#f
                                                                                           . #f)))
                 model-id
               ) ;gemini-build-stream-endpoint
             ) ;url
            ) ;
        (values url (json->string req-body) proxy-alist)
      ) ;let*
    ) ;define

    (define (%perform-http-stream url req-json-str headers proxy-alist stream-cb
              progress eff-idle
            ) ;%perform-http-stream
      (catch #t
        (lambda ()
          (http-post url
            :data            req-json-str
            :headers         headers
            :proxy           proxy-alist
            :stream          #t
            :callback        stream-cb
            :idle-timeout    eff-idle
            :connect-timeout PI_DEFAULT_CONNECT_TIMEOUT_SECONDS
            :progress        progress
          ) ;http-post
        ) ;lambda
        (lambda (tag . args)
          (if (eq? tag 'http-client-timeout-error)
            (let* ((raw-msg (if (pair? args) (car args) ""))
                   (msg (pi-format-timeout-reason raw-msg eff-idle))
                  ) ;
              (list (cons 'status-code 0)
                (cons 'reason msg)
                (cons 'text "")
                (cons 'timeout #t)
              ) ;list
            ) ;let*
            (let ((err-msg (if (pair? args) (car args) "Network error")))
              (list (cons 'status-code 0) (cons 'reason err-msg) (cons 'text ""))
            ) ;let
          ) ;if
        ) ;lambda
      ) ;catch
    ) ;define

    ;; 重试用尽后的错误结果：额度耗尽时给出 provider 官方消息，否则用状态码描述
    (define (%retry-exhausted-result stream-state status resp provider err-body
              model-id on-event
            ) ;%retry-exhausted-result
      (gemini-stream-stop-active-block! stream-state on-event)
      (let ((limit (pi-usage-limit-error? status err-body provider)))
        (pi-assistant-message (gemini-stream-state-blocks stream-state)
          :stop-reason "error"
          :usage       (gemini-stream-state-usage stream-state)
          :error-message
          (if limit
            (pi-usage-limit-error-message limit provider)
            (format-failed-stream-msg status resp)
          ) ;if
          :provider provider
          :model    model-id
          :api      "google-generative-ai"
        ) ;pi-assistant-message
      ) ;let
    ) ;define

    (define (%log-http-error status body provider config attempt max-attempts url)
      (pi-log-http-error! status
        body
        `((provider . ,provider)
          (model . ,(pi-cfg-ref config 'model-id))
          (url . ,(string-append "POST " url))
          (attempt . ,attempt)
          (max-attempts . ,max-attempts)
          (retryable . ,(gemini-is-retriable-error? status body provider)))
      ) ;pi-log-http-error!
    ) ;define
    (define (%log-finish-reason-error stream-state body provider config attempt
              max-attempts url
            ) ;%log-finish-reason-error
      (gemini-log-finish-reason-error! (gemini-stream-state-finish-reason stream-state)
        body
        provider
        (pi-cfg-ref config 'model-id)
        url
        attempt
        max-attempts
      ) ;gemini-log-finish-reason-error!
    ) ;define

    (define (%retry-stream-request resp status attempt max-retries base-delay-ms
              max-agent-delay-ms
            ) ;%retry-stream-request
      (let* ((ra-raw (pi-extract-retry-after resp))
             (ra-sec (pi-parse-retry-after ra-raw))
             (delay-sec (pi-calc-retry-delay attempt base-delay-ms max-agent-delay-ms ra-sec)
             ) ;delay-sec
             (retry-status (if (%resp-field resp 'timeout) (%resp-field resp 'reason) status)
             ) ;retry-status
            ) ;
        (pi-log-retry retry-status attempt max-retries delay-sec ra-sec)
        (sleep delay-sec)
      ) ;let*
    ) ;define
    (define (%run-gemini-stream-loop config url req-json-str proxy-alist
              on-event effective-idle provider model-id key-rotation?
              max-retries base-delay-ms max-agent-delay-ms provider-max-retries
            ) ;%run-gemini-stream-loop
      (let loop
        ((attempt 1) (api-key (pi-request-api-key config)))
        (let* ((eff-headers (list (cons "Content-Type" "application/json")
                              (cons "x-goog-api-key" (or api-key ""))
                            ) ;list
               ) ;eff-headers
               (stream-state (make-gemini-stream-state '() #f (gemini-map-usage '()) '() #f #f #f #f)
               ) ;stream-state
               (idle-timed-out? #f)
               (aborted? #f)
               (emitted? #f)
               (wrapped-on-event (if (procedure? on-event) (lambda (ev) (set! emitted? #t) (on-event ev)) #f)
               ) ;wrapped-on-event
               (error-body-acc (vector ""))
               (last-act (vector (pi-current-monotonic-seconds)))
               (stream-cb (%make-stream-callback stream-state
                            wrapped-on-event
                            effective-idle
                            (lambda () (set! idle-timed-out? #t))
                            (lambda () (set! aborted? #t))
                            error-body-acc
                            last-act
                          ) ;%make-stream-callback
               ) ;stream-cb
               (progress (pi-make-stream-progress-cb effective-idle
                           last-act
                           (lambda () (set! idle-timed-out? #t))
                           (lambda () (set! aborted? #t))
                         ) ;pi-make-stream-progress-cb
               ) ;progress
               (resp (%perform-http-stream url req-json-str eff-headers
                       proxy-alist stream-cb progress effective-idle
                     ) ;%perform-http-stream
               ) ;resp
               (status (or (%resp-field resp 'status-code) 0))
               (err-body (vector-ref error-body-acc 0))
              ) ;
          ;; 异常请求（非 200 / 空闲超时 / 200 但 finishReason 映射为 error）
          ;; 连同原始响应落盘，便于事后排障
          (when
            (and (not aborted?)
              (not (pi-aborted?))
              (or idle-timed-out?
                (not (= status 200))
                (equal? (gemini-map-finish-reason (or (gemini-stream-state-finish-reason stream-state) "STOP")
                          #f
                        ) ;gemini-map-finish-reason
                  "error"
                ) ;equal?
              ) ;or
            ) ;and
            (if (= status 200)
              (%log-finish-reason-error stream-state err-body provider config
                attempt provider-max-retries url
              ) ;%log-finish-reason-error
              (%log-http-error status err-body provider config attempt
                provider-max-retries url
              ) ;%log-http-error
            ) ;if
          ) ;when
          (cond
            ;; 用户中止优先：不进入 key 轮换 / 重试，直接结算 aborted；
            ;; 流尾补探测 pi-aborted? 覆盖单 chunk 交付时信号无后续检查点的场景
            ((or aborted? (pi-aborted?))
             (%finalize-aborted-stream stream-state provider model-id)
            ) ;
            ;; HTTP 200 但 finishReason=MALFORMED_RESPONSE：服务端生成中途
            ;; 偶发校验失败，重发通常即恢复；受 provider 重试上限约束
            ((and (= status 200)
               (not idle-timed-out?)
               (gemini-retriable-finish-reason? (gemini-stream-state-finish-reason stream-state)
               ) ;gemini-retriable-finish-reason?
             ) ;and
             (if (<= attempt provider-max-retries)
               (begin
                 (%retry-stream-request resp
                   (gemini-stream-state-finish-reason stream-state)
                   attempt
                   provider-max-retries
                   base-delay-ms
                   max-agent-delay-ms
                 ) ;%retry-stream-request
                 (loop (+ attempt 1) api-key)
               ) ;begin
               (gemini-finalize-stream-success stream-state wrapped-on-event
                 provider model-id "google-generative-ai"
               ) ;gemini-finalize-stream-success
             ) ;if
            ) ;
            ((and (not idle-timed-out?)
               key-rotation?
               (not emitted?)
               (or (= status 429) (= status 403))
             ) ;and
             (let ((next (pi-rotate-api-key! config api-key status)))
               (if (eq? next 'exhausted)
                 (gemini-classify-stream-result stream-state idle-timed-out?
                   resp status effective-idle wrapped-on-event error-body-acc
                   provider model-id "google-generative-ai"
                 ) ;gemini-classify-stream-result
                 (loop 1 next)
               ) ;if
             ) ;let
            ) ;
            ((or idle-timed-out? (gemini-is-retriable-error? status err-body provider))
             (cond
              ((<= attempt provider-max-retries)
               (%retry-stream-request
                 (if idle-timed-out?
                   `((timeout . #t)
                     (reason
                       . ,(string-append "Stream idle timed out after "
                            (number->string effective-idle)
                            "s")))
                   resp
                 ) ;if
                 status
                 attempt
                 provider-max-retries
                 base-delay-ms
                 max-agent-delay-ms
               ) ;%retry-stream-request
               (loop (+ attempt 1) api-key)
              ) ;
              (idle-timed-out? (gemini-classify-stream-result stream-state
                                 idle-timed-out? resp status effective-idle
                                 wrapped-on-event error-body-acc provider
                                 model-id "google-generative-ai"
                               ) ;gemini-classify-stream-result
              ) ;idle-timed-out?
              (else (%retry-exhausted-result stream-state status resp provider
                      err-body model-id wrapped-on-event
                    ) ;%retry-exhausted-result
              ) ;else
             ) ;cond
            ) ;
            (else (gemini-classify-stream-result stream-state idle-timed-out?
                    resp status effective-idle wrapped-on-event error-body-acc
                    provider model-id "google-generative-ai"
                  ) ;gemini-classify-stream-result
            ) ;else
          ) ;cond
        ) ;let*
      ) ;let
    ) ;define

    (define* (gemini-request-stream config
               messages
               (system-instruction #f)
               (tool-names #f)
               (on-event #f)
               (timeout #f)
               (opt-retry #f)
               (idle-timeout #f)
             ) ;gemini-request-stream
      (let-values (((url req-json-str proxy-alist)
                    (%build-stream-req-params config tool-names messages system-instruction)
                   ) ;
                  ) ;
        (let* ((settings (pi-cfg-ref config 'settings))
               (provider (pi-cfg-ref config 'provider "google"))
               (model-id (pi-cfg-ref config 'model-id))
               (effective-idle (pi-resolve-stream-idle-timeout settings timeout idle-timeout))
               (key-rotation? (pi-key-rotation? config))
              ) ;
          (let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
                        (pi-resolve-retry-params settings opt-retry)
                       ) ;
                      ) ;
            (%run-gemini-stream-loop config url req-json-str proxy-alist
              on-event effective-idle provider model-id key-rotation?
              max-retries base-delay-ms max-agent-delay-ms provider-max-retries
            ) ;%run-gemini-stream-loop
          ) ;let-values
        ) ;let*
      ) ;let-values
    ) ;define*

  ) ;begin
) ;define-library
