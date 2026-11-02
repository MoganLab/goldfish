;; gemini-map-finish-reason
;; 映射 Gemini 的 finishReason 到 pi agent 的 stopReason。
;;
;; 语法
;; ----
;; (gemini-map-finish-reason finish-reason has-function-call?)
;;
;; 参数
;; ----
;; finish-reason : string | symbol
;; Gemini 返回的 finishReason，例如 STOP、MAX_TOKENS。
;; has-function-call? : boolean
;; 本次响应是否包含函数调用。
;;
;; 返回值
;; ----
;; string
;; 标准 stopReason，可取 stop、length、toolUse 或 error。

(import (liii check) (liii pi ai api google-generative-ai))

(check-set-mode! 'report-failed)

(check (gemini-map-finish-reason "STOP" #f) => "stop")
(check (gemini-map-finish-reason "STOP" #t) => "toolUse")
(check (gemini-map-finish-reason "MAX_TOKENS" #f) => "length")
(check (gemini-map-finish-reason "SAFETY" #f) => "error")
(check (gemini-map-finish-reason "OTHER" #f) => "error")

(check-report)
