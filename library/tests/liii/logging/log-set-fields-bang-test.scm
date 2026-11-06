;; log-set-fields!
;; 设置全局日志字段（plist 形态）。
;;
;; 语法
;; ----
;; (log-set-fields! plist)
;;
;; 参数
;; ----
;; plist : list
;; 全局字段，键值交替的 plist，例如 '(APP "test")。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 全局字段会附加到每一条发送出去的日志消息上。

(import (liii check) (liii logging))

(check-set-mode! 'report-failed)

(log-set-fields! '(APP "test"))
(check (current-log-fields) => '(APP "test"))

;; 发送日志时全局字段随消息一起送出
(define *last-msg* #f)
(log-set-callback! (lambda (msg) (set! *last-msg* msg)))
(log-set-level! DEBUG)
(log-info "with fields")
(check (cdr (assq 'APP *last-msg*)) => "test")

;; 恢复默认
(log-set-fields! '())
(check (current-log-fields) => '())

(check-report)
