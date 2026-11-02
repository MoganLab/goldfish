;; gemini-map-finish-reason
;; 将 Gemini 响应的 finishReason 映射为 pi agent 标准 stopReason。
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

(import (liii check)
  (liii pi ai api google-generative-ai core)
) ;import

(check-set-mode! 'report-failed)

;; STOP 且没有函数调用
(check (gemini-map-finish-reason "STOP" #f) => "stop")

;; STOP 且包含函数调用
(check (gemini-map-finish-reason "STOP" #t) => "toolUse")

;; MAX_TOKENS 截断
(check (gemini-map-finish-reason "MAX_TOKENS" #f) => "length")
(check (gemini-map-finish-reason "MAX_TOKENS" #t) => "length")

;; 其它原因映射为 error
(check (gemini-map-finish-reason "SAFETY" #f) => "error")
(check (gemini-map-finish-reason "RECITATION" #f) => "error")
(check (gemini-map-finish-reason "unknown" #f) => "error")

;; symbol 形态的 finishReason
(check (gemini-map-finish-reason 'STOP #f) => "stop")
(check (gemini-map-finish-reason 'MAX_TOKENS #f) => "length")

(check-report)
