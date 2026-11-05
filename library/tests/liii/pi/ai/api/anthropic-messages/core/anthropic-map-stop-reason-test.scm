;; anthropic-map-stop-reason
;; 将 Anthropic 响应的 stop_reason 映射为 pi agent 标准 stopReason。
;;
;; 语法
;; ----
;; (anthropic-map-stop-reason reason)
;;
;; 参数
;; ----
;; reason : string | symbol
;; Anthropic 返回的 stop_reason，例如 end_turn、max_tokens、tool_use。
;;
;; 返回值
;; ----
;; string
;; 标准 stopReason，可取 stop、length、toolUse 或 error。

(import (liii check) (liii pi ai api anthropic-messages core))

(check-set-mode! 'report-failed)

(check (anthropic-map-stop-reason "end_turn") => "stop")
(check (anthropic-map-stop-reason "max_tokens") => "length")
(check (anthropic-map-stop-reason "tool_use") => "toolUse")
(check (anthropic-map-stop-reason "refusal") => "error")
(check (anthropic-map-stop-reason "pause_turn") => "stop")
(check (anthropic-map-stop-reason "stop_sequence") => "stop")
(check (anthropic-map-stop-reason "sensitive") => "error")
(check (anthropic-map-stop-reason "unknown") => "error")

;; symbol 形态的 stop_reason
(check (anthropic-map-stop-reason 'end_turn) => "stop")

(check-report)
