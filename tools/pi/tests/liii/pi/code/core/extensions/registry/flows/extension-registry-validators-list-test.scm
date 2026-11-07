;; extension-registry-validators-list
;; 列出注册表中已注册的全部参数校验过程。
;;
;; 语法
;; ----
;; (extension-registry-validators-list target)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; 返回值
;; ----
;; list
;; 校验过程列表。

(import (scheme base)
  (liii check)
  (liii pi code core extensions registry flows)
  (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (check (extension-registry-validators-list reg) => '())
  (register-validator! reg (lambda (parsed) #t))
  (check (length (extension-registry-validators-list reg)) => 1)
) ;let

(check-report)
