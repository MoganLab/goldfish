;; extension-command-def-description
;; 读取命令定义中的描述字符串。
;;
;; 语法
;; ----
;; (extension-command-def-description cmd)
;;
;; 参数
;; ----
;; cmd : extension-command-def
;; 命令定义记录。
;;
;; 返回值
;; ----
;; any
;; 描述字符串。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(let ((cmd (make-extension-command-def "c" "d" (lambda (a x) 1))))
  (check (extension-command-def-description cmd) => "d")
) ;let

(check-report)
