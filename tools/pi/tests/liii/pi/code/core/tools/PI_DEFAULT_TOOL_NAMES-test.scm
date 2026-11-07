;; PI_DEFAULT_TOOL_NAMES
;; 默认启用的内置工具名单表
;;
;; 语法
;; ----
;; (PI_DEFAULT_TOOL_NAMES ...)
;;
;; 参数
;; ----
;; 见各断言场景：本测试按用例组织，覆盖典型入参。
;;
;; 返回值
;; ------
;; 见各断言场景：按被测函数实际返回值断言。

(import (liii check) (liii pi code core tools))

(check-set-mode! 'report-failed)

;; 默认集不含扩展工具，扩展 default-active 由注册表声明
(check PI_DEFAULT_TOOL_NAMES => '("read" "bash" "edit" "write"))
(check (length PI_ALL_TOOL_NAMES) => 7)
(check-true (not (member "web_search" PI_ALL_TOOL_NAMES)))

(check-report)
