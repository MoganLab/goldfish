;; extension-tool-def-execute
;; 读取工具定义中的执行过程。
;;
;; 语法
;; ----
;; (extension-tool-def-execute tool)
;;
;; 参数
;; ----
;; tool : extension-tool-def
;; 工具定义记录。
;;
;; 返回值
;; ----
;; any
;; 执行过程。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(let ((tool (make-extension-tool-def "t" "d" '((type . "object")) (lambda (a c) 1))))
  (check-true (procedure? (extension-tool-def-execute tool))))

(check-report)
