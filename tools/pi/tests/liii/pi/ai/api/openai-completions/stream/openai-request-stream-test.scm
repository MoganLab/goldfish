(import (liii check) (liii pi ai api openai-completions) (liii pi ai message))

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
;;
;; 示例
;; ----
;; (openai-request-stream config messages #f '("bash") on-event 10)

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

(check-report)
