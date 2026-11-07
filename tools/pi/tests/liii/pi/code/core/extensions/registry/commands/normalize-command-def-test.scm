;; normalize-command-def
;; 将命令定义统一规范化为扩展命令定义记录。
;;
;; 语法
;; ----
;; (normalize-command-def def)
;;
;; 参数
;; ----
;; def : (or extension-command-def alist)
;; 命令定义记录或字段 alist。
;;
;; 返回值
;; ----
;; extension-command-def
;; 规范化后的命令定义记录。

(import (scheme base)
  (liii check)
  (liii pi code core extensions registry commands)
  (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let ((cmd (make-extension-command-def "c" "d" (lambda (a x) 1))))
  (check (normalize-command-def cmd) => cmd)
) ;let

(let ((cmd
        (normalize-command-def
          (list (cons 'name "c") (cons 'description "d") (cons 'handler (lambda (a x) 1)))
        ) ;normalize-command-def
      ) ;cmd
     ) ;
  (check-true (extension-command-def? cmd))
  (check (extension-command-def-name cmd) => "c")
  (check (extension-command-def-description cmd) => "d")
) ;let

(check-report)
