;; gemini-is-retriable-error?
;; 判断 Gemini 响应是否属于可重试错误（pi-is-retriable-error? 的别名）。
;;
;; 语法
;; ----
;; (gemini-is-retriable-error? status-code response-body-str)
;; (gemini-is-retriable-error? status-code response-body-str provider)
;;
;; 参数
;; ----
;; status-code : integer?
;; HTTP 状态码。
;;
;; response-body-str : string?
;; 响应体原文。
;;
;; provider : string?
;; 可选，服务商 id。
;;
;; 返回值
;; ----
;; boolean
;; 可重试返回 #t，否则 #f。

(import (liii check) (liii pi ai retry))

(check-set-mode! 'report-failed)

(check (gemini-is-retriable-error? 429 "") => #t)
(check (gemini-is-retriable-error? 503 "") => #t)
(check (gemini-is-retriable-error? 404 "") => #f)

(check-report)
