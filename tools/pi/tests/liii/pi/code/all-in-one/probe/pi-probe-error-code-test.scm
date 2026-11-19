;; pi-probe-error-code
;; 从预检失败文本或异常信息中提取统一错误码。
;;
;; 语法
;; ----
;; (pi-probe-error-code message)
;;
;; 参数
;; ----
;; message : any
;; 请求返回的错误文本、异常对象或已是错误码本身；非字符串按原样透传。
;;
;; 返回值
;; ----
;; (or integer symbol)
;; HTTP 状态码；无状态码时按文本返回 timeout 或 error。

(import (scheme base) (liii check) (liii pi code all-in-one probe))

(check-set-mode! 'report-failed)

(check (pi-probe-error-code "Error: HTTP 529 after 3 attempts: overloaded")
  =>
  529
) ;check
(check (pi-probe-error-code "HTTP 429: quota exceeded") => 429)
(check (pi-probe-error-code "Error: HTTP 401 Unauthorized") => 401)
(check (pi-probe-error-code "probe timeout after 8000ms") => 'timeout)
(check (pi-probe-error-code "connection refused") => 'error)
(check (pi-probe-error-code #f) => 'error)

(check-report)
