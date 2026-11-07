;; extension-registry-listeners
;; 读取扩展注册表中的事件监听器列表。
;;
;; 语法
;; ----
;; (extension-registry-listeners reg)
;;
;; 参数
;; ----
;; reg : extension-registry
;; 扩展注册表记录。
;;
;; 返回值
;; ----
;; list
;; 事件监听器列表。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types core)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (check (extension-registry-listeners reg) => '()))

(check-report)
