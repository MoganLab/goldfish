;; extension-registry-validators
;; 读取扩展注册表中的参数校验器列表。
;;
;; 语法
;; ----
;; (extension-registry-validators reg)
;;
;; 参数
;; ----
;; reg : extension-registry
;; 扩展注册表记录。
;;
;; 返回值
;; ----
;; list
;; 参数校验器列表。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi code core extensions types core)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (check (extension-registry-validators reg) => '())
) ;let

(check-report)
