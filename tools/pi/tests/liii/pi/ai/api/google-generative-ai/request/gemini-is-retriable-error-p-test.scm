;; gemini-is-retriable-error?
;; 判断 Gemini 协议的失败是否属于可重试错误。
;;
;; 语法
;; ----
;; (gemini-is-retriable-error? status-code response-body-str)
;; (gemini-is-retriable-error? status-code response-body-str provider)
;;
;; 参数
;; ----
;; status-code : integer
;; HTTP 状态码；0 表示网络错误/连接失败。
;; response-body-str : string?
;; 响应体原文，用于交给 provider 的额度耗尽识别函数。
;; provider : string?
;; 可选，服务商 id；省略时不做额度耗尽判定。
;;
;; 返回值
;; ----
;; boolean
;; 可重试返回 #t，否则 #f。
;;
;; 注意
;; ----
;; 可重试集合：0、408、409、429 与所有大于等于 500 的状态码。
;; 额度耗尽优先判为不可重试，其余 4xx 一律不可重试。

(import (liii check)
  (liii pi ai api google-generative-ai request)
) ;import

(check-set-mode! 'report-failed)

(check (gemini-is-retriable-error? 200 "{}") => #f)
(check (gemini-is-retriable-error? 400 "{}") => #f)
(check (gemini-is-retriable-error? 408 "{}") => #t)
(check (gemini-is-retriable-error? 409 "{}") => #t)
(check (gemini-is-retriable-error? 429 "{}") => #t)
(check (gemini-is-retriable-error? 500 "{}") => #t)
(check (gemini-is-retriable-error? 503 "{}") => #t)
(check (gemini-is-retriable-error? 0 "") => #t)

(check-report)
