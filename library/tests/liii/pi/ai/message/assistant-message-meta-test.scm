;; assistant-message-meta
;; 验证助手消息结构体元数据读写与 alist 序列化。
;;
;; 语法
;; ----
;; (pi-assistant-message content ... :provider prov :model model :api api)
;;
;; 参数
;; ----
;; provider : string? 或 #f
;; 服务商标识（如 "minimax-cn", "google", "anthropic"）
;;
;; model : string? 或 #f
;; 模型标识（如 "MiniMax-M2.7", "gemini-2.5-flash"）
;;
;; api : string? 或 #f
;; 协议接口标识（如 "anthropic-messages", "google-generative-ai"）
;;
;; 返回值
;; ----
;; pi-assistant-message?
;; 包含元数据的助手消息实例。

(import (liii check) (liii pi ai message) (liii pi ai types))

(check-set-mode! 'report-failed)

;; 1. 默认元数据字段为 #f
(let ((msg (pi-assistant-message "hello")))
  (check-true (pi-assistant-message? msg))
  (check (pi-assistant-message-provider msg) => #f)
  (check (pi-assistant-message-model msg) => #f)
  (check (pi-assistant-message-api msg) => #f)
  (let ((al (pi-message->alist msg)))
    (check (assoc "provider" al) => #f)
    (check (assoc "model" al) => #f)
    (check (assoc "api" al) => #f)
  ) ;let
) ;let

;; 2. 指定 provider, model, api 关键字参数
(let ((msg (pi-assistant-message (list (pi-text-block "thinking response"))
             :stop-reason "stop"
             :provider    "minimax-cn"
             :model       "MiniMax-M2.7"
             :api         "anthropic-messages"
           ) ;pi-assistant-message
      ) ;msg
     ) ;
  (check-true (pi-assistant-message? msg))
  (check (pi-assistant-message-provider msg) => "minimax-cn")
  (check (pi-assistant-message-model msg) => "MiniMax-M2.7")
  (check (pi-assistant-message-api msg) => "anthropic-messages")
  (let ((al (pi-message->alist msg)))
    (check (cdr (assoc "provider" al)) => "minimax-cn")
    (check (cdr (assoc "model" al)) => "MiniMax-M2.7")
    (check (cdr (assoc "api" al)) => "anthropic-messages")
    (check (cdr (assoc "role" al)) => "assistant")
    (check (cdr (assoc "stopReason" al)) => "stop")
  ) ;let
) ;let

;; 3. 验证 pi-make-assistant-message 包装函数支持元数据
(let ((msg (pi-make-assistant-message "content" "toolUse" #f "google"
             "gemini-2.5-flash" "google-generative-ai"
           ) ;pi-make-assistant-message
      ) ;msg
     ) ;
  (check (pi-assistant-message-provider msg) => "google")
  (check (pi-assistant-message-model msg) => "gemini-2.5-flash")
  (check (pi-assistant-message-api msg) => "google-generative-ai")
) ;let

(check-report)
