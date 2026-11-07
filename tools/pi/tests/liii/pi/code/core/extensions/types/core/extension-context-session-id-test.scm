;; extension-context-session-id
;; 读取扩展上下文中的会话标识。
;;
;; 语法
;; ----
;; (extension-context-session-id ctx)
;;
;; 参数
;; ----
;; ctx : extension-context
;; 扩展上下文记录。
;;
;; 返回值
;; ----
;; string 或 #f
;; 会话标识，未设置时为 #f。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types core)
) ;import

(check-set-mode! 'report-failed)

(check (extension-context-session-id (make-extension-context "sess-1")) => "sess-1")
(check (extension-context-session-id (make-extension-context)) => #f)

(check-report)
