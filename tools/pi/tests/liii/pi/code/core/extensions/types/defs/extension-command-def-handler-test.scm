;; extension-command-def-handler
;; 读取命令定义中的处理过程。
;;
;; 语法
;; ----
;; (extension-command-def-handler cmd)
;;
;; 参数
;; ----
;; cmd : extension-command-def
;; 命令定义记录。
;;
;; 返回值
;; ----
;; any
;; 处理过程。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(let ((cmd (make-extension-command-def "c" "d" (lambda (a x) 1))))
  (check-true (procedure? (extension-command-def-handler cmd)))
) ;let

(check-report)
