;; claude-settings-path
;; TODO: 补充 claude-settings-path 的简要描述。
;;
;; 语法
;; ----
;; (claude-settings-path ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldcode))

(check-set-mode! 'report-failed)

;; TODO: 补充 claude-settings-path 的断言用例。
(check-true (procedure? claude-settings-path))

(check-report)
