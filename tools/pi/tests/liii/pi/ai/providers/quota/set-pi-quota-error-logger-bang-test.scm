;; set-pi-quota-error-logger!
;; 设置额度解析失败时的日志记录器回调过程。
;;
;; 语法
;; ----
;; (set-pi-quota-error-logger! logger)
;;
;; 参数
;; ----
;; logger : procedure?
;; 接收日志信息的回调过程；为 #f 时静默。
;;
;; 返回值
;; ----
;; unspecified

(import (liii check) (liii pi ai providers quota))

(check-set-mode! 'report-failed)

(let ((logged #f))
  (set-pi-quota-error-logger! (lambda (msg) (set! logged msg)))
  (pi-quota-error-message "invalid json {")
  (check-true (string? logged))
  (set-pi-quota-error-logger! #f)
) ;let

(check-report)
