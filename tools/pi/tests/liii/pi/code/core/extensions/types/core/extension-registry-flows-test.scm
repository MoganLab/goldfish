;; extension-registry-flows
;; 读取扩展注册表中的流程定义列表。
;;
;; 语法
;; ----
;; (extension-registry-flows reg)
;;
;; 参数
;; ----
;; reg : extension-registry
;; 扩展注册表记录。
;;
;; 返回值
;; ----
;; list
;; 流程定义列表。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi code core extensions types core)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (check (extension-registry-flows reg) => '())
) ;let

(check-report)
