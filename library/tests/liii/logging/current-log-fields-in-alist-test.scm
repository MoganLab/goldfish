;; current-log-fields-in-alist
;; 用 alist 形态设置全局日志字段。
;;
;; 语法
;; ----
;; (current-log-fields-in-alist alist)
;;
;; 参数
;; ----
;; alist : list
;; 形如 ((ENV . "prod")) 的字段列表。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 内部转换为 plist 形态保存，与 log-set-fields! 等价。

(import (liii check) (liii logging))

(check-set-mode! 'report-failed)

(current-log-fields-in-alist '((ENV . "prod")))
(check (current-log-fields) => '(ENV "prod"))

;; 发送日志时字段随消息一起送出
(define *last-msg* #f)
(log-set-callback! (lambda (msg) (set! *last-msg* msg)))
(log-set-level! DEBUG)
(log-info "alist fields")
(check (cdr (assq 'ENV *last-msg*)) => "prod")

;; 恢复默认
(log-set-fields! '())
(check (current-log-fields) => '())

(check-report)
