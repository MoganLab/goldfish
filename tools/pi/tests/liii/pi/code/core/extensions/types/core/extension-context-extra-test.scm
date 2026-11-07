;; extension-context-extra
;; 读取扩展上下文中的附加数据。
;;
;; 语法
;; ----
;; (extension-context-extra ctx)
;;
;; 参数
;; ----
;; ctx : extension-context
;; 扩展上下文记录。
;;
;; 返回值
;; ----
;; list
;; 附加数据列表，缺省为空列表。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types core)
) ;import

(check-set-mode! 'report-failed)

(check (extension-context-extra (make-extension-context)) => '())
(check (extension-context-extra (make-extension-context #f "." #f #f '((k . 1)))) => '((k . 1)))

(check-report)
