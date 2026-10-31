;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api openai-completions stream)
  (import (scheme base)
    (scheme write)
    (liii string)
    (liii json)
    (liii http)
    (liii time)
    (liii pi abort)
    (liii pi ai time)
    (liii pi ai event)
    (liii pi ai retry)
    (liii pi ai limits)
    (liii pi ai keyring)
    (liii pi ai message)
    (liii pi ai api openai-completions core)
    (liii pi ai api openai-completions msg)
    (liii pi ai api openai-completions request)
    (liii pi ai api openai-completions stream-parse)
    (liii pi ai api openai-completions stream-state)
  ) ;import
  (export openai-request-stream)
  (begin

    (define (%resp-field resp key)
      (cond ((procedure? resp) (resp key))
            ((hash-table? resp) (resp key))
            ((pair? resp) (cdr (or (assoc key resp) '(#f . #f))))
            (else #f)
      ) ;cond
    ) ;define

    (define (%build-stream-cb stream-state on-event effective-idle on-idle on-abort err-acc)
      (let* ((last-act (pi-current-monotonic-seconds))
             (decoder (make-openai-stream-decoder stream-state on-event))
            ) ;
        (lambda (chunk)
          (cond
            ;; 用户中止（ESC）：返回 #f 令 C 层中止 HTTP 下载
            ((pi-aborted?) (on-abort) #f)
            (else
              (let ((now (pi-current-monotonic-seconds)))
                (vector-set! err-acc 0 (string-append (vector-ref err-acc 0) chunk))
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
            ) ;else
          ) ;cond
        ) ;lambda
      ) ;let*
    ) ;define

    (define (%finalize-stream stream-state)
      (finish-openai-stream stream-state #f)
      (let* ((blocks (openai-stream-state-blocks stream-state))
             (stop-reason (openai-stream-state-stop-reason stream-state))
             (err-msg (openai-stream-state-error-message stream-state))
            ) ;
        (pi-assistant-message blocks
          :stop-reason   stop-reason
          :usage         (openai-stream-state-usage stream-state)
          :error-message err-msg
        ) ;pi-assistant-message
      ) ;let*
    ) ;define

    ;; 用户中止（ESC）：保留已收到的部分内容，stop-reason 强制为 aborted
    (define (%finalize-aborted-stream stream-state)
      (finish-openai-stream stream-state #f)
      (pi-assistant-message (openai-stream-state-blocks stream-state)
        :stop-reason   "aborted"
        :usage         (openai-stream-state-usage stream-state)
        :error-message (openai-stream-state-error-message stream-state)
      ) ;pi-assistant-message
    ) ;define

    (define (%make-idle-timeout-error eff-idle)
      (pi-assistant-message '()
        :stop-reason "error"
        :usage       (openai-map-usage '())
        :error-message
        (string-append "OpenAI stream idle timeout after "
          (number->string eff-idle)
          " seconds"
        ) ;string-append
      ) ;pi-assistant-message
    ) ;define

    (define (%extract-openai-err-msg body status)
      (let ((parsed-msg (extract-openai-stream-error-message body)))
        (if (and (string? parsed-msg) (not (string-null? parsed-msg)))
          (string-append "HTTP " (number->string status) ": " parsed-msg)
          (string-append "HTTP " (number->string status) " stream error")
        ) ;if
      ) ;let
    ) ;define

    ;; 流式响应中的错误正文：优先取回调累积的 chunk，其次取 http 响应体
    (define (%stream-error-body resp err-acc)
      (let ((acc-body (vector-ref err-acc 0)) (resp-body (or (%resp-field resp 'text) "")))
        (cond
         ((and (string? acc-body) (not (string-null? acc-body))) acc-body)
         ((and (string? resp-body) (not (string-null? resp-body))) resp-body)
         (else "")
        ) ;cond
      ) ;let
    ) ;define

    (define (%make-stream-error status resp err-acc provider)
      (let* ((resp-err (or (%resp-field resp 'reason) (%resp-field resp 'error) ""))
             (err-text (if (string? resp-err) (string-trim resp-err) ""))
             (acc-body (vector-ref err-acc 0))
             (body (%stream-error-body resp err-acc))
             (limit (pi-usage-limit-error? status body))
             (err-msg
               (cond
                ((and limit (string? (pi-usage-limit-error-message limit provider)))
                 (pi-usage-limit-error-message limit provider)
                ) ;
                ((and (zero? status) (not (string-null? err-text)))
                 (string-append "Request failed: " err-text)
                ) ;
                ((not (string-null? acc-body)) (%extract-openai-err-msg acc-body status))
                (else (string-append "HTTP " (number->string status) " stream error"))
               ) ;cond
             ) ;err-msg
            ) ;
        (pi-assistant-message '()
          :stop-reason   "error"
          :usage         (openai-map-usage '())
          :error-message err-msg
        ) ;pi-assistant-message
      ) ;let*
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

    (define (%init-openai-stream-state)
      (make-openai-stream-state '() #f (openai-map-usage '()) "stop" #f '())
    ) ;define

    (define (%run-stream-loop config url req-json-str proxy-alist on-ev timeout
              opt-retry eff-idle
            ) ;%run-stream-loop
      (let* ((settings (cdr (or (assoc 'settings config) '(#f))))
             (key-rotation? (pi-key-rotation? config))
             (provider (cdr (or (assoc 'provider config) '(#f . #f))))
            ) ;
        (let-values (((max-r base-delay max-delay provider-r)
                      (pi-resolve-retry-params settings opt-retry)
                     ) ;
                    ) ;
          (let loop
            ((attempt 1) (api-key (pi-request-api-key config)))
            (let* ((headers (openai-build-headers api-key))
                   (stream-state (%init-openai-stream-state))
                   (idle-timed-out? #f)
                   (emitted? #f)
                   (aborted? #f)
                   (wrapped-on-ev (if (procedure? on-ev) (lambda (ev) (set! emitted? #t) (on-ev ev)) #f)
                   ) ;wrapped-on-ev
                   (err-acc (vector ""))
                   (cb (%build-stream-cb stream-state
                         wrapped-on-ev
                         eff-idle
                         (lambda () (set! idle-timed-out? #t))
                         (lambda () (set! aborted? #t))
                         err-acc
                       ) ;%build-stream-cb
                   ) ;cb
                   (resp (%post-stream url req-json-str headers proxy-alist cb timeout eff-idle))
                   (status (or (%resp-field resp 'status-code) 0))
                  ) ;
              ;; 流尾补探测一次中止信号：单 chunk 交付时 on-event 发出的 abort
              ;; 无后续 chunk 检查点，此处探测保证确定性结算为 aborted
              (cond ((or aborted? (pi-aborted?)) (%finalize-aborted-stream stream-state))
                    (idle-timed-out? (%make-idle-timeout-error eff-idle))
                    ;; 额度耗尽（5 小时/周额度）属于账户级限额，换 key 也无济于事，
                    ;; 直接给出可读提示，不再重试。
                    ((and (pi-usage-limit-error? status (%stream-error-body resp err-acc))
                       (not key-rotation?)
                     ) ;and
                     (%make-stream-error status resp err-acc provider)
                    ) ;
                    ((and key-rotation? (not emitted?) (or (= status 429) (= status 403)))
                     (let ((next (pi-rotate-api-key! config api-key status)))
                       (if (eq? next 'exhausted)
                         (%make-stream-error status resp err-acc provider)
                         (loop 1 next)
                       ) ;if
                     ) ;let
                    ) ;
                    ((openai-is-retriable-error? status (%stream-error-body resp err-acc))
                     (if (<= attempt provider-r)
                       (let* ((ra-raw (pi-extract-retry-after resp))
                              (ra-sec (pi-parse-retry-after ra-raw))
                              (delay-sec (pi-calc-retry-delay attempt base-delay max-delay ra-sec))
                             ) ;
                         (pi-log-retry status attempt provider-r delay-sec ra-sec)
                         (sleep delay-sec)
                         (loop (+ attempt 1) api-key)
                       ) ;let*
                       (%make-stream-error status resp err-acc provider)
                     ) ;if
                    ) ;
                    ((or (zero? status) (not (= status 200)))
                     (%make-stream-error status resp err-acc provider)
                    ) ;
                    (else (%finalize-stream stream-state))
              ) ;cond
            ) ;let*
          ) ;let
        ) ;let-values
      ) ;let*
    ) ;define

    (define* (openai-request-stream config
               messages
               (system-instruction #f)
               (tool-names #f)
               (on-event #f)
               (timeout 300)
               (opt-retry #f)
               (idle-timeout #f)
             ) ;openai-request-stream
      (let* ((model-id (cdr (assoc 'model-id config)))
             (model-meta (cdr (assoc 'model-meta config)))
             (proxy-url (cdr (assoc 'proxy-url config)))
             (base-url (cdr (or (assoc 'base-url model-meta) '(#f
                                                               . "https://api.openai.com/v1")))
             ) ;base-url
             (max-tokens (cdr (or (assoc 'max-tokens model-meta) '(#f . 8192))))
             (url (openai-build-endpoint base-url))
             (eff-tool-names (or tool-names (cdr (assoc 'tools config))))
             (tool-resolver (if (assoc 'tool-resolver config)
                              (cdr (assoc 'tool-resolver config))
                              (lambda (name) #f)
                            ) ;if
             ) ;tool-resolver
             (tool-decls (openai-build-tool-declarations-from-names eff-tool-names tool-resolver)
             ) ;tool-decls
             (openai-msgs (openai-build-messages messages system-instruction))
             (req-body (openai-build-request-body model-id openai-msgs #t tool-decls max-tokens)
             ) ;req-body
             (req-json-str (json->string req-body))
             (proxy-alist (openai-build-proxy-alist proxy-url))
             (eff-idle (or idle-timeout timeout))
            ) ;
        (%run-stream-loop config url req-json-str proxy-alist on-event timeout
          opt-retry eff-idle
        ) ;%run-stream-loop
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
