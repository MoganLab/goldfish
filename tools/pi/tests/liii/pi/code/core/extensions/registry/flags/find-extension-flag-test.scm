;; find-extension-flag
;; 按标志名或别名查找注册表中已注册的标志。
;;
;; 语法
;; ----
;; (find-extension-flag target flag-name)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; flag-name : string
;; 标志名或别名。
;;
;; 返回值
;; ----
;; (or extension-flag-def #f)
;; 匹配的标志定义记录，未找到时返回 #f。

(import (scheme base)
  (liii check)
  (liii pi code core extensions registry flags)
  (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let* ((reg (make-extension-registry))
       (flag (make-extension-flag-def "--verbose" "d" :aliases '("-v")))
      ) ;
  (register-flag! reg flag)
  (check (find-extension-flag reg "--verbose") => flag)
  (check (find-extension-flag reg "-v") => flag)
  (check (find-extension-flag reg "missing") => #f)
) ;let*

(check-report)
