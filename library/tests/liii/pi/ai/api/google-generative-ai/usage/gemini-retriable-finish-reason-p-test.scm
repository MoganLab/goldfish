;; gemini-retriable-finish-reason?
;; 判断 Gemini 的 finishReason 是否属于值得原样重发请求的可重试失败。
;;
;; 语法
;; ----
;; (gemini-retriable-finish-reason? finish-reason)
;;
;; 参数
;; ----
;; finish-reason : string | #f
;; 流式/非流式响应结算时拿到的原始 finishReason。
;;
;; 返回值
;; ----
;; boolean
;; MALFORMED_RESPONSE（大小写不敏感）返回 #t；其余（SAFETY、
;; RECITATION、STOP、#f 等）返回 #f。
;;
;; 说明
;; ----
;; MALFORMED_RESPONSE 是服务端生成中途的偶发内部校验失败，重发通常即可
;; 成功；SAFETY/RECITATION 等属于内容判定类终态，重发无意义。

(import (liii check) (liii pi ai api google-generative-ai usage))

(check-set-mode! 'report-failed)

;; MALFORMED_RESPONSE 可重试
(check (gemini-retriable-finish-reason? "MALFORMED_RESPONSE") => #t)

;; 大小写不敏感
(check (gemini-retriable-finish-reason? "malformed_response") => #t)
(check (gemini-retriable-finish-reason? "Malformed_Response") => #t)

;; 其它 finishReason 不重试
(check (gemini-retriable-finish-reason? "MALFORMED_FUNCTION_CALL") => #f)
(check (gemini-retriable-finish-reason? "SAFETY") => #f)
(check (gemini-retriable-finish-reason? "RECITATION") => #f)
(check (gemini-retriable-finish-reason? "STOP") => #f)
(check (gemini-retriable-finish-reason? "") => #f)

;; 非字符串输入不重试
(check (gemini-retriable-finish-reason? #f) => #f)
(check (gemini-retriable-finish-reason? 'MALFORMED_RESPONSE) => #f)

(check-report)
