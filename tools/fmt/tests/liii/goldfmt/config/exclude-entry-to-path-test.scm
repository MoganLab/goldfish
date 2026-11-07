;; exclude-entry->path
;; TODO: 补充 exclude-entry->path 的简要描述。
;;
;; 语法
;; ----
;; (exclude-entry->path ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt config))

(check-set-mode! 'report-failed)

;; TODO: 补充 exclude-entry->path 的断言用例。
(check-true (procedure? exclude-entry->path))

(check-report)
