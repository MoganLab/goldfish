;; fmt-cache-path
;; TODO: 补充 fmt-cache-path 的简要描述。
;;
;; 语法
;; ----
;; (fmt-cache-path ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt cache))

(check-set-mode! 'report-failed)

;; TODO: 补充 fmt-cache-path 的断言用例。
(check-true (procedure? fmt-cache-path))

(check-report)
