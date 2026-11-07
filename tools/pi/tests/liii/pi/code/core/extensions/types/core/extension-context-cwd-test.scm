;; extension-context-cwd
;; 读取扩展上下文中的工作目录。
;;
;; 语法
;; ----
;; (extension-context-cwd ctx)
;;
;; 参数
;; ----
;; ctx : extension-context
;; 扩展上下文记录。
;;
;; 返回值
;; ----
;; string
;; 工作目录路径。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types core)
) ;import

(check-set-mode! 'report-failed)

(check (extension-context-cwd (make-extension-context)) => ".")
(check (extension-context-cwd (make-extension-context #f "/tmp/work")) => "/tmp/work")

(check-report)
