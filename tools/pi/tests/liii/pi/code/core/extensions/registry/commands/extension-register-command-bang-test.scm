;; extension-register-command!
;; 向注册表注册命令定义（register-command! 的别名）。
;;
;; 语法
;; ----
;; (extension-register-command! target command-def)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; command-def : (or extension-command-def alist)
;; 命令定义。
;;
;; 返回值
;; ----
;; extension-command-def
;; 规范化后注册的命令定义记录。

(import (scheme base)
  (liii check)
  (liii pi code core extensions registry commands)
  (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let* ((reg (make-extension-registry))
       (cmd
         (extension-register-command! reg
           (make-extension-command-def "c" "d" (lambda (a x) 1))
         ) ;extension-register-command!
       ) ;cmd
      ) ;
  (check (length (extension-registry-commands reg)) => 1)
  (check (extension-command-def-name cmd) => "c")
) ;let*

(check-report)
