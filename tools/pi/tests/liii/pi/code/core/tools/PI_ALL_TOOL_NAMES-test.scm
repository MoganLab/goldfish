;; PI_ALL_TOOL_NAMES
;; TODO: 补充 PI_ALL_TOOL_NAMES 的简要描述。
;;
;; 语法
;; ----
;; (PI_ALL_TOOL_NAMES ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii pi code core tools))

(check-set-mode! 'report-failed)

;; 工具名单表内容断言
(check (length PI_ALL_TOOL_NAMES) => 7)
(check (car PI_ALL_TOOL_NAMES) => "read")
(check-true (not (not (member "bash" PI_ALL_TOOL_NAMES))))

(check-report)
