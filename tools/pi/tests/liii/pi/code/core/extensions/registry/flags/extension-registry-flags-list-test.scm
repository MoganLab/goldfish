;; extension-registry-flags-list
;; 列出注册表中已注册的全部命令行标志。
;;
;; 语法
;; ----
;; (extension-registry-flags-list target)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; 返回值
;; ----
;; list
;; 标志定义记录列表。

(import (scheme base)
  (liii check)
  (liii pi code core extensions registry flags)
  (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (check (extension-registry-flags-list reg) => '())
  (register-flag! reg (make-extension-flag-def "--f" "d"))
  (check (length (extension-registry-flags-list reg)) => 1)
) ;let

(check-report)
