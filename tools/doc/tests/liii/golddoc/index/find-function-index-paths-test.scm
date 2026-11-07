;; find-function-index-paths
;; TODO: 补充 find-function-index-paths 的简要描述。
;;
;; 语法
;; ----
;; (find-function-index-paths ...)
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

;; TODO: 补充 find-function-index-paths 的断言用例。
(check-true (procedure? find-function-index-paths))

(check-report)
