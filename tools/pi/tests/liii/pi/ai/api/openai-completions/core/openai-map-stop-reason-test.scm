;; openai-map-stop-reason
;; 将 Kimi/OpenAI 响应 finish_reason 映射到 pi agent 标准 stopReason。
;;
;; 语法
;; ----
;; (openai-map-stop-reason reason)
;;
;; 参数
;; ----
;; reason : string | symbol
;; OpenAI 返回的 finish_reason，例如 stop、length、tool_calls。
;;
;; 返回值
;; ----
;; string
;; 标准 stopReason，可取 stop、length、toolUse 或 error。

(import (liii check) (liii pi ai api openai-completions))

(check-set-mode! 'report-failed)

(check (openai-map-stop-reason "stop") => "stop")
(check (openai-map-stop-reason "length") => "length")
(check (openai-map-stop-reason "tool_calls") => "toolUse")
(check (openai-map-stop-reason "content_filter") => "error")
(check (openai-map-stop-reason "unknown") => "error")

(check-report)
