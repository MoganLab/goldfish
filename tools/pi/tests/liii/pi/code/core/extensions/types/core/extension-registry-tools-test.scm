;; extension-registry-tools
;; 读取扩展注册表中的工具定义列表。
;;
;; 语法
;; ----
;; (extension-registry-tools reg)
;;
;; 参数
;; ----
;; reg : extension-registry
;; 扩展注册表记录。
;;
;; 返回值
;; ----
;; list
;; 工具定义列表。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types core)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (check (extension-registry-tools reg) => '()))

(check-report)
