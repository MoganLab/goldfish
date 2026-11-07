;; extension-registry-commands-list
;; 列出注册表中已注册的全部命令。
;;
;; 语法
;; ----
;; (extension-registry-commands-list target)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; 返回值
;; ----
;; list
;; 命令定义记录列表。

(import (scheme base)
        (liii check)
        (liii pi code core extensions registry commands)
        (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (check (extension-registry-commands-list reg) => '())
  (register-command! reg (make-extension-command-def "c" "d" (lambda (a x) 1)))
  (check (length (extension-registry-commands-list reg)) => 1)
) ;let*

(check-report)
