;; extension-tool-def-guidelines
;; 读取工具定义中的使用指引列表。
;;
;; 语法
;; ----
;; (extension-tool-def-guidelines tool)
;;
;; 参数
;; ----
;; tool : extension-tool-def
;; 工具定义记录。
;;
;; 返回值
;; ----
;; any
;; 使用指引列表。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(let ((tool (make-extension-tool-def "t" "d" '((type . "object")) (lambda (a c) 1))))
  (check (extension-tool-def-guidelines tool) => '()))

(check-report)
