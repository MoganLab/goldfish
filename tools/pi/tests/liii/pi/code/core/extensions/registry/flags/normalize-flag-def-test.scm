;; normalize-flag-def
;; 将标志定义统一规范化为扩展标志定义记录。
;;
;; 语法
;; ----
;; (normalize-flag-def def)
;;
;; 参数
;; ----
;; def : (or extension-flag-def alist)
;; 标志定义记录或字段 alist。
;;
;; 返回值
;; ----
;; extension-flag-def
;; 规范化后的标志定义记录。

(import (scheme base)
        (liii check)
        (liii pi code core extensions registry flags)
        (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let ((flag (make-extension-flag-def "--f" "d")))
  (check (normalize-flag-def flag) => flag)
) ;let*

(let ((flag (normalize-flag-def (list (cons 'flag "--f") (cons 'description "d")))))
  (check-true (extension-flag-def? flag))
  (check (extension-flag-def-flag flag) => "--f")
  (check (extension-flag-def-description flag) => "d")
) ;let*

(check-report)
