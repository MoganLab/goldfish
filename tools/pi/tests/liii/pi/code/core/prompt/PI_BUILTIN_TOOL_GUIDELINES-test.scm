;; PI_BUILTIN_TOOL_GUIDELINES
;; 内置工具使用指引常量。
;;
;; 语法
;; ----
;; PI_BUILTIN_TOOL_GUIDELINES
;;
;; 参数
;; ----
;; 无（常量）
;;
;; 返回值
;; ------
;; string
;; 内置工具使用指引文本。

(import (liii check)
  (liii pi code core prompt)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

;; PI_BUILTIN_TOOL_GUIDELINES
;; 内置工具在系统提示词规则节中的规范指引映射表。
(check-true (pair? PI_BUILTIN_TOOL_GUIDELINES))
(check-true (pair? (assoc "read" PI_BUILTIN_TOOL_GUIDELINES)))

(check-report)
