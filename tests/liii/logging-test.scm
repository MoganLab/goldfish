(import (liii check) (liii string) (liii logging))

;; (liii logging) 测试用例

;; 用于接收回调结果的全局变量
(define *last-msg* #f)

;; 日志级别常量
(check EMERGENCY => 0)
(check ALERT => 1)
(check CRITICAL => 2)
(check ERROR => 3)
(check WARNING => 4)
(check NOTICE => 5)
(check INFO => 6)
(check DEBUG => 7)

;; current-log-fields 默认值为空 plist
(check (current-log-fields) => '())

;; current-log-callback 默认值是过程
(check (procedure? (current-log-callback)) => #t)

;; send-log 兼容 SRFI-215
(set! *last-msg* #f)
(log-set-callback! (lambda (msg) (set! *last-msg* msg)))
(send-log INFO "srfi-215 compat")
(check (cdr (assq 'SEVERITY *last-msg*)) => 6)
(check (cdr (assq 'MESSAGE *last-msg*)) => "srfi-215 compat")

;; log-set-fields! 和 log-set-callback!
(set! *last-msg* #f)
(log-set-fields! '(APP "test"))
(log-set-callback! (lambda (msg) (set! *last-msg* msg)))
(send-log INFO "with fields")
(check (cdr (assq 'APP *last-msg*)) => "test")

;; current-log-fields-in-alist
(set! *last-msg* #f)
(current-log-fields-in-alist '((ENV . "prod")))
(send-log INFO "alist fields")
(check (cdr (assq 'ENV *last-msg*)) => "prod")

;; 恢复默认
(log-set-fields! '())

;; 便捷函数 log-info
(set! *last-msg* #f)
(log-set-callback! (lambda (msg) (set! *last-msg* msg)))
(log-info "info message")
(check (cdr (assq 'SEVERITY *last-msg*)) => 6)
(check (cdr (assq 'MESSAGE *last-msg*)) => "info message")

;; 便捷函数 log-debug
(set! *last-msg* #f)
(log-debug "debug message")
(check (cdr (assq 'SEVERITY *last-msg*)) => 7)

;; 便捷函数 log-error
(set! *last-msg* #f)
(log-error "error message")
(check (cdr (assq 'SEVERITY *last-msg*)) => 3)

;; 便捷函数支持 pyfmt 格式化
(set! *last-msg* #f)
(log-info "User %(name)s logged in" :name "Alice")
(check (cdr (assq 'MESSAGE *last-msg*)) => "User Alice logged in")

;; log-set-level! 过滤低级别日志
(set! *last-msg* #f)
(log-set-level! INFO)
(log-debug "should be filtered")
(check *last-msg* => #f)

(log-info "should pass filter")
(check (cdr (assq 'MESSAGE *last-msg*)) => "should pass filter")

;; 恢复默认级别
(log-set-level! DEBUG)

;; current-log-format 默认值
(check (string? (current-log-format)) => #t)

;; log-set-format! 设置格式
(log-set-format! "[%(levelname)s] %(message)s")
(check (current-log-format) => "[%(levelname)s] %(message)s")

;; 恢复默认格式
(log-set-format! "%(asctime)s [%(levelname)s] %(message)s")

;; default-log-handler 输出到端口
(let ((p (open-output-string)))
  (default-log-handler '((SEVERITY . 6) (MESSAGE . "test")) p)
  (let ((output (get-output-string p)))
    (check (string-contains? output "test") => #t)
    (check (string-contains? output "INFO") => #t)
  ) ;let
) ;let

;; make-stdout-handler 和 make-stderr-handler
(check (procedure? (make-stdout-handler)) => #t)
(check (procedure? (make-stderr-handler)) => #t)

;; 辅助函数
(check (log-message-severity '((SEVERITY . 3) (MESSAGE . "err"))) => 3)
(check (log-message-message '((SEVERITY . 3) (MESSAGE . "err"))) => "err")
(check (log-message-field '((SEVERITY . 3) (CODE . 500)) 'CODE) => 500)
(check (log-message-field '((SEVERITY . 3)) 'MISSING) => #f)

;; 恢复默认回调
(log-set-callback! (lambda (log-entry) (values)))

(check-report)
