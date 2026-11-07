;; extension-tool-def-default-active?
;; 读取工具定义中的默认启用状态。
;;
;; 语法
;; ----
;; (extension-tool-def-default-active? tool)
;;
;; 参数
;; ----
;; tool : extension-tool-def
;; 工具定义记录。
;;
;; 返回值
;; ----
;; any
;; 默认启用状态。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(let ((tool (make-extension-tool-def "t" "d" '((type . "object")) (lambda (a c) 1))))
  (check (extension-tool-def-default-active? tool) => #f)
) ;let

(check-report)
