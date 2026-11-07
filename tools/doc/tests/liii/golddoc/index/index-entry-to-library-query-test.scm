;; index-entry->library-query
;; TODO: 补充 index-entry->library-query 的简要描述。
;;
;; 语法
;; ----
;; (index-entry->library-query ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii golddoc index))

(check-set-mode! 'report-failed)

;; TODO: 补充 index-entry->library-query 的断言用例。
(check-true (procedure? index-entry->library-query))

(check-report)
