;; set-extension-registry-augmenters!
;; 更新扩展注册表中的配置增强器列表。
;;
;; 语法
;; ----
;; (set-extension-registry-augmenters! reg value)
;;
;; 参数
;; ----
;; reg : extension-registry
;; 扩展注册表记录。
;;
;; value : list
;; 新的配置增强器列表。
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
  (set-extension-registry-augmenters! reg '(a))
  (check (extension-registry-augmenters reg) => '(a))
) ;let

(check-report)
