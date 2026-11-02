;; set-pi-retry-logger!
;; 设置自定义重试日志记录器过程。
;;
;; 语法
;; ----
;; (set-pi-retry-logger! proc)
;;
;; 参数
;; ----
;; proc : procedure? 或 #f
;; 接收单参数消息字符串的回调过程；为 #f 时恢复默认行为。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base) (liii check) (liii pi ai retry))

(check-set-mode! 'report-failed)

(let ((logged #f))
  (set-pi-retry-logger! (lambda (msg) (set! logged msg)))
  (pi-log-retry 429 1 3 2.0)
  (check-true (string? logged))
  (set-pi-retry-logger! #f)
) ;let

(check-report)
