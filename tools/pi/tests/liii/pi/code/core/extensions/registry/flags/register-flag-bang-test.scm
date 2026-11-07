;; register-flag!
;; 向注册表注册命令行标志定义，同名标志按标志名覆盖。
;;
;; 语法
;; ----
;; (register-flag! target flag-def)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; flag-def : (or extension-flag-def alist)
;; 标志定义。
;;
;; 返回值
;; ----
;; extension-flag-def
;; 规范化后注册的标志定义记录。

(import (scheme base)
        (liii check)
        (liii pi code core extensions registry flags)
        (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let* (
       (reg (make-extension-registry))
       (flag (register-flag! reg (make-extension-flag-def "--f" "d"))))
  (check (length (extension-registry-flags reg)) => 1)
  (check (extension-flag-def-flag flag) => "--f")
) ;let*

(check-report)
