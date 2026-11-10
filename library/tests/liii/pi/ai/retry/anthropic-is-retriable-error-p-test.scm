;; anthropic-is-retriable-error?
;; 判断 Anthropic 响应是否属于可重试错误（pi-is-retriable-error? 的别名）。
;;
;; 语法
;; ----
;; (anthropic-is-retriable-error? status-code response-body-str)
;; (anthropic-is-retriable-error? status-code response-body-str provider)
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

(check (anthropic-is-retriable-error? 429 "") => #t)
(check (anthropic-is-retriable-error? 500 "") => #t)
(check (anthropic-is-retriable-error? 400 "") => #f)

(check-report)
