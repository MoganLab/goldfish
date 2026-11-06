;; PI_BUILTIN_TOOL_SNIPPETS
;; 内置工具代码片段常量。
;;
;; 语法
;; ----
;; PI_BUILTIN_TOOL_SNIPPETS
;;
;; 参数
;; ----
;; 无（常量）
;;
;; 返回值
;; ------
;; string
;; 内置工具代码片段文本。

(import (liii check) (liii pi code core prompt) (liii pi code core session-manager))

(check-set-mode! 'report-failed)

;; PI_BUILTIN_TOOL_SNIPPETS
;; 内置工具在系统提示词中的简短功能描述映射表。
(check-true (pair? PI_BUILTIN_TOOL_SNIPPETS))
(check (cdr (assoc "read" PI_BUILTIN_TOOL_SNIPPETS)) => "Read file contents")
(check (cdr (assoc "bash" PI_BUILTIN_TOOL_SNIPPETS)) => "Execute bash commands (ls, grep, find, etc.)")

(check-report)
