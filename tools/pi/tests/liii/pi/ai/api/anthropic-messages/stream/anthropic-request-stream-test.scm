(import (liii check)
  (liii string)
  (liii list)
  (liii path)
  (liii subprocess)
  (liii go)
  (liii time)
  (scheme time)
  (liii pi abort)
  (liii pi ai event)
  (liii pi ai api anthropic-messages)
) ;import

;; anthropic-request-stream
;; 向 Anthropic Messages API 发起流式请求，并将响应转换为内部 assistant 消息。
;;
;; 语法
;; ----
;; (anthropic-request-stream config messages [system-instruction [tool-names [on-event [timeout [opt-retry [idle-timeout]]]]]])
;;
;; 参数
;; ----
;; config : alist
;; 包含 provider、model-id、api-key、model-meta 和重试配置等请求参数。
;; messages : list
;; 内部会话消息列表。
;; system-instruction : string | #f，可选
;; 系统提示词。
;; tool-names : list | #f，可选
;; 允许模型调用的工具名列表。
;; on-event : procedure | #f，可选
;; 接收流式事件的回调。
;; timeout : number，可选，默认为 300
;; 请求超时秒数。
;; opt-retry : alist | #f，可选
;; 本次请求的重试覆盖配置。
;; idle-timeout : number | #f，可选
;; 流空闲超时秒数；为 #f 时使用 timeout。
;;
;; 返回值
;; ----
;; alist | pi-assistant-message
;; 包含 content、stopReason 和 usage 的 assistant 消息；失败时包含 errorMessage。
;;
;; 说明
;; ----
;; 支持文本、工具调用和 thinking 流式块，以及重试、空闲超时和用户中止。
;;
;; 示例
;; ----
;; (anthropic-request-stream config messages #f '("bash") on-event 10)

(check-set-mode! 'report-failed)

;; 本地 stub server 集成测试

(define (parse-port-line line)
  (if (and (string? line) (string-starts? line "PORT:"))
    (string->number (string-trim-both (substring line 5 (string-length line))))
    #f
  ) ;if
) ;define

(define stub-server-file
  (if (file-exists? "tools/pi/tests/stub_gemini_server.py")
    "tools/pi/tests/stub_gemini_server.py"
    "tests/stub_gemini_server.py"
  ) ;if
) ;define

;; 延迟发送 abort 信号的 worker：0.5s 后向通道发中止信号。
;; 顶层 define + (go (fn ch)) 传参形式（闭包变量无法跨 go 会话传输）
(define (delay-abort-signal ch)
  (sleep 0.5)
  (chan-send! ch 'abort 0)
) ;define

(let* ((proc (process-spawn (list "python3" stub-server-file) :stdout 'pipe))
       (port-line (process-read-line proc))
       (port (parse-port-line port-line))
       (base-url (string-append "http://127.0.0.1:" (number->string port)))
      ) ;
  (check-true (number? port))

  (let ((config
          `((provider . "anthropic")
            (model-id . "claude-3-7-sonnet-20250219")
            (api-key . "sk-ant-test-key")
            (model-meta (base-url . ,base-url) (max-tokens . 4096))
            (proxy-url . #f)
            (tools "bash")
            (settings (retry (enabled . #t)
                        (max-retries . 3)
                        (base-delay-ms . 10)
                        (max-agent-delay-ms . 50))))
        ) ;config
       ) ;

    ;; Anthropic SSE 流式请求与 text_delta 事件捕获
    (let* ((evs '())
           (stream-cfg
             `((provider . "anthropic")
               (model-id . "claude-3-7-sonnet-20250219")
               (api-key . "sk-ant-test-key")
               (model-meta (base-url . ,(string-append base-url "/stream"))
                 (max-tokens . 4096))
               (proxy-url . #f)
               (tools "bash")
               (settings))
           ) ;stream-cfg
           (on-ev (lambda (ev) (set! evs (cons ev evs))))
           (resp
             (anthropic-request-stream stream-cfg
               '(((role . "user") (content . "stream please")))
               #f
               '("bash")
               on-ev
               10
             ) ;anthropic-request-stream
           ) ;resp
          ) ;
      (check (cdr (assoc 'role resp)) => "assistant")
      (check (cdr (assoc 'stopReason resp)) => "stop")
      (let* ((content (cdr (assoc 'content resp))) (first-blk (car content)))
        (check (cdr (assoc 'type first-blk)) => "text")
        (check (cdr (assoc 'text first-blk)) => "Hello from Claude stream!")
      ) ;let*
      (let ((delta-evs
              (filter
                (lambda (e) (equal? (cdr (assoc 'type e)) "text_delta"))
                evs
              ) ;filter
            ) ;delta-evs
           ) ;
        (check (> (length delta-evs) 0) => #t)
      ) ;let
    ) ;let*

    ;; Anthropic 兼容网关流式（含 thinking 块）
    (let* ((evs '())
           (compat-stream-cfg
             `((provider . "anthropic")
               (model-id . "glm-5-turbo")
               (api-key . "sk-ant-test-key")
               (model-meta (base-url
                             . ,(string-append base-url "/compat-gateway"))
                 (max-tokens . 4096))
               (proxy-url . #f)
               (tools "bash")
               (settings))
           ) ;compat-stream-cfg
           (on-ev (lambda (ev) (set! evs (cons ev evs))))
           (resp
             (anthropic-request-stream compat-stream-cfg
               '(((role . "user") (content . "compat stream")))
               #f
               '("bash")
               on-ev
               10
             ) ;anthropic-request-stream
           ) ;resp
          ) ;
      (check (cdr (assoc 'role resp)) => "assistant")
      (check (cdr (assoc 'stopReason resp)) => "stop")
      (let* ((content (cdr (assoc 'content resp)))
             (first-blk (car content))
             (second-blk (cadr content))
            ) ;
        (check (length content) => 2)
        (check (cdr (assoc 'type first-blk)) => "thinking")
        (check (cdr (assoc 'thinking first-blk)) => "Thinking process...")
        (check (cdr (assoc 'thinkingSignature first-blk)) => "sig_abc")
        (check (cdr (assoc 'type second-blk)) => "text")
        (check (cdr (assoc 'text second-blk)) => "Hello from compatible gateway!")
      ) ;let*
      (let ((u (cdr (assoc 'usage resp))))
        (check (cdr (assoc 'input u)) => 15)
        (check (cdr (assoc 'output u)) => 8)
        (check (cdr (assoc 'cacheRead u)) => 4)
      ) ;let
    ) ;let*

    ;; 400 报错时返回 stopReason=error 并包含错误信息
    (let* ((err-stream-cfg
             `((provider . "anthropic")
               (model-id . "fake-model")
               (api-key . "sk-ant-test-key")
               (model-meta (base-url . ,(string-append base-url "/error-400"))
                 (max-tokens . 4096))
               (proxy-url . #f)
               (tools "bash")
               (settings))
           ) ;err-stream-cfg
           (resp
             (anthropic-request-stream err-stream-cfg
               '(((role . "user") (content . "cause 400")))
               #f
               '("bash")
               #f
               10
             ) ;anthropic-request-stream
           ) ;resp
          ) ;
      (check (cdr (assoc 'stopReason resp)) => "error")
      (check-true (string-contains? (cdr (assoc 'errorMessage resp)) "model not found")
      ) ;check-true
    ) ;let*

    ;; 空闲期中止：stub 发 1 个 chunk 后静默挂起 30s，模拟 LLM 思考期。
    ;; 首个流事件到达时同步发送 abort 信号，之后无新 chunk，
    ;; 无 progress 节拍探测时只能等到 timeout（红灯）；
    ;; 接入 :progress 后约 1s 节拍内中止并结算 aborted（绿灯）
    (let* ((ch (make-chan 1))
           (t0 (current-second))
           (hang-cfg
             `((provider . "anthropic")
               (model-id . "claude-3-7-sonnet-20250219")
               (api-key . "sk-ant-test-key")
               (model-meta (base-url . ,(string-append base-url "/stream-abort-hang"))
                 (max-tokens . 4096))
               (proxy-url . #f)
               (tools "bash")
               (settings))
           ) ;hang-cfg
           (resp
             (begin
               (pi-abort-arm-chan! ch)
               ;; 信号晚于 chunk1 到达：chunk 检查点探不到，只能靠 progress 节拍探测
               (go (delay-abort-signal ch))
               (anthropic-request-stream hang-cfg
                 '(((role . "user") (content . "hang then abort")))
                 #f
                 '("bash")
                 #f
                 60
               ) ;anthropic-request-stream
             ) ;begin
           ) ;resp
           (dt (- (current-second) t0))
          ) ;
      (check-true (< dt 10))
      (check (cdr (assoc 'stopReason resp)) => "aborted")
      (pi-abort-reset!)
    ) ;let*
  ) ;let

  (process-kill proc)
) ;let*

(check-report)
