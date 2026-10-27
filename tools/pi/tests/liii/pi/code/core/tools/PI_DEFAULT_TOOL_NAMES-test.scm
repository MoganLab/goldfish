(import (liii check)
  (liii pi code core tools)
) ;import

(check-set-mode! 'report-failed)

;; 默认集不含扩展工具，扩展 default-active 由注册表声明
(check PI_DEFAULT_TOOL_NAMES => '("read" "bash" "edit" "write"))
(check (length PI_ALL_TOOL_NAMES) => 7)
(check-true (not (member "web_search" PI_ALL_TOOL_NAMES)))

(check-report)
