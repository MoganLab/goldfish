;; current-log-callback
;; 读取当前生效的日志回调。
;;
;; 语法
;; ----
;; (current-log-callback)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; procedure
;; 当前日志回调，接收一条日志 alist。
;;
;; 说明
;; ----
;; 默认回调把日志输出到标准错误流；
;; 可用 log-set-callback! 替换为自定义回调。

(import (liii check) (liii logging))

(check-set-mode! 'report-failed)

(check (procedure? (current-log-callback)) => #t)

;; 替换后可读回新回调
(let ((cb (lambda (msg) #t)))
  (log-set-callback! cb)
  (check (eq? (current-log-callback) cb) => #t)
  ;; 恢复默认
  (log-set-callback! (make-stderr-handler))
) ;let

(check-report)
