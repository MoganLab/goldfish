;; pi-user-warn
;; 向标准输出打印警告文本。
;;
;; 语法
;; ----
;; (pi-user-warn msg)
;;
;; 参数
;; ----
;; msg : string
;; 警告文本。
;;
;; 返回值
;; ----
;; 不返回
;; 向标准输出打印 "[warn] " 前缀的警告行。

(import (liii check)
  (liii pi code config user-fs)
) ;import

(check-set-mode! 'report-failed)

;; 1. 验证为过程对象
(check-true (procedure? pi-user-warn))

;; 2. 正常调用不抛错
(pi-user-warn "config missing")
(check-true #t)

(check-report)
