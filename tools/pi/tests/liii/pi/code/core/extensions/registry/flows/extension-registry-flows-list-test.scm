;; extension-registry-flows-list
;; 列出注册表中已注册的全部流程。
;;
;; 语法
;; ----
;; (extension-registry-flows-list target)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; 返回值
;; ----
;; list
;; 流程定义记录列表。

(import (scheme base)
  (liii check)
  (liii pi code core extensions registry flows)
  (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (check (extension-registry-flows-list reg) => '())
  (register-flow! reg
    (make-extension-flow-def 'f (lambda (p) #t) (lambda (a b c d) 0))
  ) ;register-flow!
  (check (length (extension-registry-flows-list reg)) => 1)
) ;let

(check-report)
