(import (liii check) (liii pi code prompt) (liii pi code session))

(check-set-mode! 'report-failed)

;; PI_BUILTIN_TOOL_GUIDELINES
;; 内置工具在系统提示词规则节中的规范指引映射表。
(check-true (pair? PI_BUILTIN_TOOL_GUIDELINES))
(check-true (pair? (assoc "read" PI_BUILTIN_TOOL_GUIDELINES)))

(check-report)
