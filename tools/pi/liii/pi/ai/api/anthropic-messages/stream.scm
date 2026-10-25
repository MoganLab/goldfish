;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api anthropic-messages stream)
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
    (liii pi ai message)
    (liii pi ai api anthropic-messages core)
    (liii pi ai api anthropic-messages request)
    (liii pi ai api anthropic-messages stream-parse)
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

    (define (%build-proxy proxy-url)
      (if (and (string? proxy-url) (not (string-null? proxy-url)))
        (list (cons "http" proxy-url) (cons "https" proxy-url))
        '()
      ) ;if
    ) ;define

    (define (%build-stream-cb stream-state on-event effective-idle on-idle on-abort err-acc)
      (let* ((last-act (pi-current-monotonic-seconds))
             (decoder (make-anthropic-stream-decoder stream-state on-event))
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
      (when (pair? (vector-ref stream-state 6))
        (anthropic-finish-thinking-block stream-state #f)
      ) ;when
      (let* ((blocks (vector-ref stream-state 0))
             (raw-stop (vector-ref stream-state 4))
             (stop-reason (anthropic-map-stop-reason (or raw-stop "end_turn")))
             (err-msg (if (equal? stop-reason "error") raw-stop #f))
            ) ;
        (pi-assistant-message blocks
          :stop-reason   stop-reason
          :usage         (vector-ref stream-state 2)
          :error-message err-msg
        ) ;pi-assistant-message
      ) ;let*
    ) ;define

    ;; 用户中止（ESC）：保留已收到的部分内容，stop-reason 强制为 aborted
    (define (%finalize-aborted-stream stream-state)
      (when (pair? (vector-ref stream-state 6))
        (anthropic-finish-thinking-block stream-state #f)
      ) ;when
      (pi-assistant-message (vector-ref stream-state 0)
        :stop-reason   "aborted"
        :usage         (vector-ref stream-state 2)
        :error-message #f
      ) ;pi-assistant-message
    ) ;define

    (define (%make-idle-timeout-error eff-idle)
      (pi-assistant-message '()
        :stop-reason "error"
        :usage       (anthropic-map-usage '())
        :error-message
        (string-append "Anthropic stream idle timeout after "
          (number->string eff-idle)
          " seconds"
        ) ;string-append
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

    (define (%make-stream-error status resp err-acc)
      (let* ((resp-err (or (%resp-field resp 'reason) (%resp-field resp 'error) ""))
             (err-text (if (string? resp-err) (string-trim resp-err) ""))
             (acc-body (vector-ref err-acc 0))
             (err-msg
               (cond
                ((and (zero? status) (not (string-null? err-text)))
                 (string-append "Request failed: " err-text)
                ) ;
                ((not (string-null? acc-body)) (%extract-anthropic-err-msg acc-body status))
                (else (string-append "HTTP " (number->string status) " stream error"))
               ) ;cond
             ) ;err-msg
            ) ;
        (pi-assistant-message '()
          :stop-reason   "error"
          :usage         (anthropic-map-usage '())
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

    (define (%init-anthropic-stream-state)
      (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f)
    ) ;define

    (define (%run-stream-loop settings url req-json-str headers proxy-alist
              on-ev timeout opt-retry eff-idle
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
                 (cb (%build-stream-cb stream-state
                       on-ev
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
                  ((anthropic-is-retriable-error? status "")
                   (if (<= attempt provider-r)
                     (let* ((ra-raw (pi-extract-retry-after resp))
                            (ra-sec (pi-parse-retry-after ra-raw))
                            (delay-sec (pi-calc-retry-delay attempt base-delay max-delay ra-sec))
                           ) ;
                       (pi-log-retry status attempt provider-r delay-sec ra-sec)
                       (sleep delay-sec)
                       (loop (+ attempt 1))
                     ) ;let*
                     (%make-stream-error status resp err-acc)
                   ) ;if
                  ) ;
                  ((or (zero? status) (not (= status 200)))
                   (%make-stream-error status resp err-acc)
                  ) ;
                  (else (%finalize-stream stream-state))
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
             (url (anthropic-build-endpoint base-url))
             (eff-tool-names (or tool-names (cdr (assoc 'tools config))))
             (tool-resolver (if (assoc 'tool-resolver config)
                              (cdr (assoc 'tool-resolver config))
                              (lambda (name) #f)
                            ) ;if
             ) ;tool-resolver
             (tool-decls (anthropic-build-tool-declarations-from-names eff-tool-names tool-resolver)
             ) ;tool-decls
             (ant-msgs (anthropic-build-messages messages))
             (raw-req-body (%build-req-body model-id ant-msgs system-instruction tool-decls max-tokens)
             ) ;raw-req-body
             (thinking-lvl (cdr (or (assoc 'thinking-level config) '(#f . #f))))
             (req-body (anthropic-inject-thinking raw-req-body thinking-lvl))
             (req-json-str (json->string req-body))
             (headers (%build-headers api-key))
             (proxy-alist (%build-proxy proxy-url))
             (settings (cdr (or (assoc 'settings config) '(#f))))
             (eff-idle (or idle-timeout timeout))
            ) ;
        (%run-stream-loop settings url req-json-str headers proxy-alist on-event
          timeout opt-retry eff-idle
        ) ;%run-stream-loop
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
