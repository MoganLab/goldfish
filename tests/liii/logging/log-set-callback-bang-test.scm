;; log-set-callback!
;; 设置日志回调。
;;
;; 语法
;; ----
;; (log-set-callback! callback)
;;
;; 参数
;; ----
;; callback : procedure
;; 接收一条日志 alist 的回调过程。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 1. 替换回调前会关闭此前由 make-file-handler 打开的文件端口。
;; 2. 默认回调为 make-stderr-handler，输出到标准错误流。

(import (liii check) (liii logging))

(check-set-mode! 'report-failed)

(define *last-msg* #f)

(log-set-callback! (lambda (msg) (set! *last-msg* msg)))
(log-set-level! DEBUG)
(log-info "custom callback")
(check (cdr (assq 'MESSAGE *last-msg*)) => "custom callback")

;; 恢复默认回调
(log-set-callback! (make-stderr-handler))
(check (procedure? (current-log-callback)) => #t)

(check-report)
