;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api anthropic-messages stream)
  (import (scheme base)
    (scheme char)
    (scheme write)
    (liii string)
    (liii json)
    (liii http)
    (liii time)
    (liii pi ai abort)
    (liii pi ai config runtime)
    (liii pi ai time)
    (liii pi ai event)
    (liii pi ai retry)
    (liii pi ai limits)
    (liii pi ai log)
    (liii pi ai message)
    (liii pi ai api anthropic-messages core)
    (liii pi ai api anthropic-messages request)
    (liii pi ai api anthropic-messages stream-parse)
    (liii pi ai api anthropic-messages stream-state)
    (liii pi ai api anthropic-messages thinking)
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
    (define (%build-req-body model-id msgs system-prompt tool-decls max-tokens)
      (append (anthropic-build-request-body model-id msgs system-prompt tool-decls max-tokens)
        (list (cons "stream" #t))
      ) ;append
    ) ;define
    (define (%build-headers api-key)
      (let ((base '(("Content-Type" . "application/json")
                    ("anthropic-version" . "2023-06-01"))
            ) ;base
           ) ;
        (if (and (string? api-key) (string-starts? api-key "Bearer "))
          (cons (cons "Authorization" api-key) base)
          (cons (cons "x-api-key" (or api-key "")) base)
        ) ;if
      ) ;let
    ) ;define

    (define (%build-proxy proxy-url)
      (if (and (string? proxy-url) (not (string-null? proxy-url)))
        `(("http" . ,proxy-url) ("https" . ,proxy-url))
        '()
      ) ;if
    ) ;define

    (define (%build-stream-cb stream-state on-event effective-idle on-idle
              on-abort err-acc last-act
            ) ;%build-stream-cb
      (let ((decoder (make-anthropic-stream-decoder stream-state on-event)))
        (lambda (chunk)
          (cond
            ;; 用户中止（ESC）：返回 #f 令 C 层中止 HTTP 下载
            ((pi-aborted?) (on-abort) #f)
            (else
              (let ((now (pi-current-monotonic-seconds)))
                (vector-set! err-acc 0 (string-append (vector-ref err-acc 0) chunk))
                (if
                  (and effective-idle (> (- now (vector-ref last-act 0)) effective-idle))
                  (begin
                    (on-idle)
                    #f
                  ) ;begin
                  (begin
                    (vector-set! last-act 0 now)
                    (decoder chunk)
                    #t
                  ) ;begin
                ) ;if
              ) ;let
            ) ;else
          ) ;cond
        ) ;lambda
      ) ;let
    ) ;define

    (define (%finalize-stream stream-state provider model-id)
      (when (anthropic-stream-state-current-thinking stream-state)
        (anthropic-finish-thinking-block stream-state #f)
      ) ;when
      (let* ((blocks (anthropic-stream-state-blocks stream-state))
             (raw-stop (anthropic-stream-state-stop-reason stream-state))
             (stop-reason (anthropic-map-stop-reason (or raw-stop "end_turn")))
             (err-msg (if (equal? stop-reason "error") raw-stop #f))
            ) ;
        (pi-assistant-message blocks
          :stop-reason   stop-reason
          :usage         (anthropic-stream-state-usage stream-state)
          :error-message err-msg
          :provider      provider
          :model         model-id
          :api           "anthropic-messages"
        ) ;pi-assistant-message
      ) ;let*
    ) ;define

    (define (%finalize-aborted-stream stream-state provider model-id)
      (when (anthropic-stream-state-current-thinking stream-state)
        (anthropic-finish-thinking-block stream-state #f)
      ) ;when
      (pi-assistant-message (anthropic-stream-state-blocks stream-state)
        :stop-reason   "aborted"
        :usage         (anthropic-stream-state-usage stream-state)
        :error-message #f
        :provider      provider
        :model         model-id
        :api           "anthropic-messages"
      ) ;pi-assistant-message
    ) ;define

    (define (%make-idle-timeout-error stream-state eff-idle provider model-id)
      (when (anthropic-stream-state-current-thinking stream-state)
        (anthropic-finish-thinking-block stream-state #f)
      ) ;when
      (pi-assistant-message (anthropic-stream-state-blocks stream-state)
        :stop-reason "error"
        :usage       (anthropic-stream-state-usage stream-state)
        :error-message
        (string-append "Anthropic stream idle timed out after "
          (number->string eff-idle)
          " seconds (timeout)"
        ) ;string-append
        :provider provider
        :model    model-id
        :api      "anthropic-messages"
      ) ;pi-assistant-message
    ) ;define

    (define (%parse-err-json body)
      (catch #t
        (lambda ()
          (let ((json-data (string->json body)))
            (and (pair? json-data)
              (let ((err (cdr (or (assoc "error" json-data) '(#f . #f)))))
                (cond
                 ((pair? err) (cdr (or (assoc "message" err) '(#f . #f))))
                 ((string? err) err)
                 (else #f)
                ) ;cond
              ) ;let
            ) ;and
          ) ;let
        ) ;lambda
        (lambda (tag . args) #f)
      ) ;catch
    ) ;define

    (define (%extract-anthropic-err-msg body status)
      (let ((parsed-msg (%parse-err-json body)))
        (if (and (string? parsed-msg) (not (string-null? parsed-msg)))
          (string-append "HTTP " (number->string status) ": " parsed-msg)
          (string-append "HTTP " (number->string status) " stream error")
        ) ;if
      ) ;let
    ) ;define
    (define (%make-stream-error stream-state status resp err-acc provider model-id)
      (when (anthropic-stream-state-current-thinking stream-state)
        (anthropic-finish-thinking-block stream-state #f)
      ) ;when
      (let* ((resp-err (or (%resp-field resp 'reason) (%resp-field resp 'error) ""))
             (err-text (if (string? resp-err) (string-trim resp-err) ""))
             (acc-body (vector-ref err-acc 0))
             (resp-body (or (%resp-field resp 'text) ""))
             (body
               (if (and (string? acc-body) (not (string-null? acc-body)))
                 acc-body
                 (if (and (string? resp-body) (not (string-null? resp-body))) resp-body "")
               ) ;if
             ) ;body
             (limit (pi-usage-limit-error? status body provider))
             (err-msg
               (cond
                ((and limit (string? (pi-usage-limit-error-message limit provider)))
                 (pi-usage-limit-error-message limit provider)
                ) ;
                ((and (zero? status) (not (string-null? err-text)))
                 (if (string-starts? err-text "Request timed out")
                   err-text
                   (string-append "Request failed: " err-text)
                 ) ;if
                ) ;
                ((not (string-null? acc-body)) (%extract-anthropic-err-msg acc-body status))
                (else (string-append "HTTP " (number->string status) " stream error"))
               ) ;cond
             ) ;err-msg
            ) ;
        (pi-assistant-message (anthropic-stream-state-blocks stream-state)
          :stop-reason   "error"
          :usage         (anthropic-stream-state-usage stream-state)
          :error-message err-msg
          :provider      provider
          :model         model-id
          :api           "anthropic-messages"
        ) ;pi-assistant-message
      ) ;let*
    ) ;define

    (define (%post-stream url req-json-str headers proxy-alist cb progress eff-idle)
      (catch #t
        (lambda ()
          (http-post url
            :data            req-json-str
            :headers         headers
            :proxy           proxy-alist
            :stream          #t
            :callback        cb
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

    (define (%init-anthropic-stream-state)
      (make-anthropic-stream-state '()
        #f
        (anthropic-map-usage '())
        '()
        "end_turn"
        #f
        #f
      ) ;make-anthropic-stream-state
    ) ;define

    (define (%wait-stream-retry! idle-timed-out? eff-idle resp status attempt
              provider-r base-delay max-delay
            ) ;%wait-stream-retry!
      (let* ((ra-raw (pi-extract-retry-after resp))
             (ra-sec (pi-parse-retry-after ra-raw))
             (delay-sec (pi-calc-retry-delay attempt base-delay max-delay ra-sec))
             (retry-status
               (cond (idle-timed-out? (string-append "Stream idle timed out after " (number->string eff-idle) "s")
                     ) ;idle-timed-out?
                     ((%resp-field resp 'timeout) (%resp-field resp 'reason))
                     (else status)
               ) ;cond
             ) ;retry-status
            ) ;
        (pi-log-retry retry-status attempt provider-r delay-sec ra-sec)
        (sleep delay-sec)
      ) ;let*
    ) ;define

    (define (%run-stream-loop settings url req-json-str headers proxy-alist
              on-ev opt-retry eff-idle provider model-id
            ) ;%run-stream-loop
      (let-values (((max-r base-delay max-delay provider-r)
                    (pi-resolve-retry-params settings opt-retry)
                   ) ;
                  ) ;
        (let loop
          ((attempt 1))
          (let* ((stream-state (%init-anthropic-stream-state))
                 (idle-timed-out? #f)
                 (aborted? #f)
                 (err-acc (vector ""))
                 (last-act (vector (pi-current-monotonic-seconds)))
                 (cb (%build-stream-cb stream-state
                       on-ev
                       eff-idle
                       (lambda () (set! idle-timed-out? #t))
                       (lambda () (set! aborted? #t))
                       err-acc
                       last-act
                     ) ;%build-stream-cb
                 ) ;cb
                 (progress (pi-make-stream-progress-cb eff-idle
                             last-act
                             (lambda () (set! idle-timed-out? #t))
                             (lambda () (set! aborted? #t))
                           ) ;pi-make-stream-progress-cb
                 ) ;progress
                 (resp (%post-stream url req-json-str headers proxy-alist cb progress eff-idle))
                 (status (or (%resp-field resp 'status-code) 0))
                ) ;
            ;; 异常请求（非 200 / 空闲超时）连同原始响应落盘，便于事后排障
            (when
              (and (not aborted?)
                (not (pi-aborted?))
                (or idle-timed-out? (not (= status 200)))
              ) ;and
              (pi-log-http-error! status
                (vector-ref err-acc 0)
                (list (cons 'provider provider)
                  (cons 'model model-id)
                  (cons 'url (string-append "POST " url))
                  (cons 'attempt attempt)
                  (cons 'max-attempts provider-r)
                  (cons 'retryable
                    (or idle-timed-out?
                      (anthropic-is-retriable-error? status (vector-ref err-acc 0) provider)
                    ) ;or
                  ) ;cons
                ) ;list
              ) ;pi-log-http-error!
            ) ;when
            ;; 流尾补探测一次中止信号：单 chunk 交付时 on-event 发出的 abort
            ;; 无后续 chunk 检查点，此处探测保证确定性结算为 aborted
            (cond ((or aborted? (pi-aborted?))
                   (%finalize-aborted-stream stream-state provider model-id)
                  ) ;
                  ((or idle-timed-out?
                     (anthropic-is-retriable-error? status (vector-ref err-acc 0) provider)
                   ) ;or
                   (cond ((<= attempt provider-r)
                          (%wait-stream-retry! idle-timed-out? eff-idle resp
                            status attempt provider-r base-delay max-delay
                          ) ;%wait-stream-retry!
                          (loop (+ attempt 1))
                         ) ;
                         (idle-timed-out? (%make-idle-timeout-error stream-state eff-idle provider model-id)
                         ) ;idle-timed-out?
                         (else (%make-stream-error stream-state status resp err-acc provider model-id))
                   ) ;cond
                  ) ;
                  ((or (zero? status) (not (= status 200)))
                   (%make-stream-error stream-state status resp err-acc provider model-id)
                  ) ;
                  (else (%finalize-stream stream-state provider model-id))
            ) ;cond
          ) ;let*
        ) ;let
      ) ;let-values
    ) ;define

    (define* (anthropic-request-stream config
               messages
               (system-instruction #f)
               (tool-names #f)
               (on-event #f)
               (timeout #f)
               (opt-retry #f)
               (idle-timeout #f)
             ) ;anthropic-request-stream
      (let* ((model-id (pi-cfg-ref config 'model-id))
             (model-meta (pi-cfg-ref config 'model-meta))
             (api-key (pi-cfg-ref config 'api-key))
             (proxy-url (pi-cfg-ref config 'proxy-url))
             (base-url (cdr (or (assoc 'base-url model-meta) '(#f
                                                               . "https://api.anthropic.com")))
             ) ;base-url
             (max-tokens (cdr (or (assoc 'max-tokens model-meta) '(#f . 8192))))
             (url (anthropic-build-endpoint base-url))
             (eff-tool-names (or tool-names (pi-cfg-ref config 'tools '())))
             (tool-resolver (or (pi-cfg-ref config 'tool-resolver) (lambda (name) #f)))
             (tool-decls (anthropic-build-tool-declarations-from-names eff-tool-names tool-resolver)
             ) ;tool-decls
             (ant-msgs (anthropic-build-messages messages))
             (raw-req-body (%build-req-body model-id ant-msgs system-instruction tool-decls max-tokens)
             ) ;raw-req-body
             (thinking-lvl (pi-cfg-ref config 'thinking-level))
             (req-body (anthropic-inject-thinking raw-req-body thinking-lvl))
             (req-json-str (json->string req-body))
             (headers (%build-headers api-key))
             (proxy-alist (%build-proxy proxy-url))
             (settings (pi-cfg-ref config 'settings))
             (provider (pi-cfg-ref config 'provider))
             (eff-idle (pi-resolve-stream-idle-timeout settings timeout idle-timeout))
            ) ;
        (%run-stream-loop settings url req-json-str headers proxy-alist on-event
          opt-retry eff-idle provider model-id
        ) ;%run-stream-loop
      ) ;let*
    ) ;define*
  ) ;begin
) ;define-library
