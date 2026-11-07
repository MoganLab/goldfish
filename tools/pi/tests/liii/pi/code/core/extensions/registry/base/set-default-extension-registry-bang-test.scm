;; set-default-extension-registry!
;; 设置进程级默认扩展注册表记录。
;;
;; 语法
;; ----
;; (set-default-extension-registry! target)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context #f)
;; 目标对象，#f 表示清空默认注册表。
;;
;; 返回值
;; ----
;; 不返回
;; 直接更新默认注册表。

(import (scheme base)
        (liii check)
        (liii pi code core extensions registry base)
        (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let* (
       (reg (make-extension-registry))
       (saved (default-extension-registry)))
  (set-default-extension-registry! reg)
  (check (default-extension-registry) => reg)
  (set-default-extension-registry! saved)
) ;let*

(check-report)
