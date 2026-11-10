;; set-extension-registry-flows!
;; 更新扩展注册表中的流程定义列表。
;;
;; 语法
;; ----
;; (set-extension-registry-flows! reg value)
;;
;; 参数
;; ----
;; reg : extension-registry
;; 扩展注册表记录。
;;
;; value : list
;; 新的流程定义列表。
;;
;; 返回值
;; ----
;; 不返回
;; 直接更新注册表记录。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi code core extensions types core)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (set-extension-registry-flows! reg '(a))
  (check (extension-registry-flows reg) => '(a))
) ;let

(check-report)
