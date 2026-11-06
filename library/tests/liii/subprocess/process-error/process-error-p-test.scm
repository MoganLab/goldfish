;; process-error?
;; 判断一个对象是否为子进程错误记录。
;;
;; 语法
;; ----
;; (process-error? object)
;;
;; 参数
;; ----
;; object : any
;; 待判断的任意对象。
;;
;; 返回值
;; ----
;; boolean
;; 对象是 process-error 记录时返回 #t，否则返回 #f。

(import (liii check) (liii subprocess process-error))

(check-set-mode! 'report-failed)

(check (process-error? (make-process-error 1 "" "" "false")) => #t)
(check (process-error? "not-an-error") => #f)
(check (process-error? 123) => #f)
(check (process-error? '()) => #f)

(check-report)
