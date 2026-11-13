;; openai-request-stream
;; 向 OpenAI Chat Completions API 发起流式请求，并返回内部 assistant 消息。
;;
;; 语法
;; ----
;; (openai-request-stream config messages [system-instruction [tool-names [on-event [timeout [opt-retry [idle-timeout]]]]]])
;;
;; 参数
;; ----
;; config : alist
;; 包含 model-id、model-meta、api-key 和重试配置等请求参数。
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
;; pi-assistant-message
;; 包含 content、stopReason 和 usage；失败时包含 errorMessage。
;;
;; 说明
;; ----
;; 支持文本与工具调用增量、自动重试、API key 轮换、空闲超时和用户中止。

(import (liii check)
  (liii string)
  (liii pi ai api openai-completions)
  (liii pi ai retry)
  (liii pi ai message)
) ;import

(check-set-mode! 'report-failed)

;; openai-request-stream 网络错误或不可达端点处理
(let* ((cfg
         (list (cons 'model-id "kimi-for-coding")
           (cons 'model-meta '(("base-url" . "http://127.0.0.1:1")
                               ("max-tokens" . 1024)))
           (cons 'api-key "dummy-key")
           (cons 'proxy-url #f)
           (cons 'tools '())
           (cons 'settings '((retry (enabled . #f))))
         ) ;list
       ) ;cfg
       (msgs (list (pi-user-message "ping")))
       (resp (openai-request-stream cfg msgs #f '() #f 10))
      ) ;
  (check (pi-assistant-message? resp) => #t)
  (check (pi-assistant-message-stop-reason resp) => "error")
  (check (string? (pi-assistant-message-error-message resp)) => #t)
) ;let*

;; openai-request-stream 客户端超时明确感知秒数（禁用重试）
(let* ((cfg
         (list (cons 'model-id "kimi-for-coding")
           (cons 'model-meta '(("base-url" . "http://192.0.2.1")
                               ("max-tokens" . 1024)))
           (cons 'api-key "dummy-key")
           (cons 'proxy-url #f)
           (cons 'tools '())
           (cons 'settings '((retry (enabled . #f))))
         ) ;list
       ) ;cfg
       (msgs (list (pi-user-message "ping")))
       (resp (openai-request-stream cfg msgs #f '() #f 0.2))
      ) ;
  (check (pi-assistant-message? resp) => #t)
  (check (pi-assistant-message-stop-reason resp) => "error")
  (check (pi-assistant-message-error-message resp) => "Request timed out after 0.2s")
) ;let*

;; openai-request-stream 超时纳入与 HTTP 429/0 一样的退避重试
(let* ((retry-logs '())
       (_ (set-pi-retry-logger! (lambda (msg) (set! retry-logs (cons msg retry-logs)))))
       (cfg
         (list (cons 'model-id "kimi-for-coding")
           (cons 'model-meta '(("base-url" . "http://192.0.2.1")
                               ("max-tokens" . 1024)))
           (cons 'api-key "dummy-key")
           (cons 'proxy-url #f)
           (cons 'tools '())
           (cons 'settings '((retry (enabled . #t) (max-retries . 2) (base-delay-ms . 10))))
         ) ;list
       ) ;cfg
       (msgs (list (pi-user-message "ping")))
       (resp (openai-request-stream cfg msgs #f '() #f 0.2))
      ) ;
  (set-pi-retry-logger! #f)
  (check (pi-assistant-message? resp) => #t)
  (check (pi-assistant-message-stop-reason resp) => "error")
  (check (pi-assistant-message-error-message resp) => "Request timed out after 0.2s")
  ;; 验证发生了 2 次重试，且日志明确携带超时秒数
  (check (length retry-logs) => 2)
  (check-true (string-contains? (car (reverse retry-logs))
                "[pi retry] Request timed out after 0.2s, attempt 1/2"
              ) ;string-contains?
  ) ;check-true
  (check-true (string-contains? (car retry-logs)
                "[pi retry] Request timed out after 0.2s, attempt 2/2"
              ) ;string-contains?
  ) ;check-true
) ;let*

(check-report)
