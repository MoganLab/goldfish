;; extension-registry-augmenters-list
;; 列出注册表中已注册的全部配置增强过程。
;;
;; 语法
;; ----
;; (extension-registry-augmenters-list target)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; 返回值
;; ----
;; list
;; 配置增强过程列表。

(import (scheme base)
  (liii check)
  (liii pi code core extensions registry flows)
  (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (check (extension-registry-augmenters-list reg) => '())
  (register-config-augmenter! reg (lambda (cfg) cfg))
  (check (length (extension-registry-augmenters-list reg)) => 1)
) ;let

(check-report)
