;; extension-registry-augmenters
;; 读取扩展注册表中的配置增强器列表。
;;
;; 语法
;; ----
;; (extension-registry-augmenters reg)
;;
;; 参数
;; ----
;; reg : extension-registry
;; 扩展注册表记录。
;;
;; 返回值
;; ----
;; list
;; 配置增强器列表。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types core)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (check (extension-registry-augmenters reg) => '()))

(check-report)
