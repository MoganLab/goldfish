;; find-extension-command
;; 按命令名查找注册表中已注册的命令。
;;
;; 语法
;; ----
;; (find-extension-command target name)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; name : string
;; 命令名。
;;
;; 返回值
;; ----
;; (or extension-command-def #f)
;; 匹配的命令定义记录，未找到时返回 #f。

(import (scheme base)
  (liii check)
  (liii pi code core extensions registry commands)
  (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let* ((reg (make-extension-registry))
       (cmd (make-extension-command-def "review" "d" (lambda (a x) 1)))
      ) ;
  (register-command! reg cmd)
  (check (find-extension-command reg "review") => cmd)
  (check (find-extension-command reg "missing") => #f)
) ;let*

(check-report)
