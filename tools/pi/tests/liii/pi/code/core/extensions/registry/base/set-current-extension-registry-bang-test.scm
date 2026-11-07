;; set-current-extension-registry!
;; 设置当前生效的扩展注册表记录。
;;
;; 语法
;; ----
;; (set-current-extension-registry! target)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context #f)
;; 目标对象，#f 表示清空当前注册表。
;;
;; 返回值
;; ----
;; (or extension-registry #f)
;; 设置后的注册表记录，清空时返回 #f。

(import (scheme base)
  (liii check)
  (liii pi code core extensions registry base)
  (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let* ((reg (make-extension-registry)) (saved (current-extension-registry)))
  (set-current-extension-registry! reg)
  (check (current-extension-registry) => reg)
  (set-current-extension-registry! #f)
  (check (current-extension-registry) => #f)
  (set-current-extension-registry! saved)
) ;let*

(check-report)
