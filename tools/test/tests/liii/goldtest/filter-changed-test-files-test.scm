;; filter-changed-test-files
;; TODO: 补充 filter-changed-test-files 的简要描述。
;;
;; 语法
;; ----
;; (filter-changed-test-files ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldtest))

(check-set-mode! 'report-failed)

;; TODO: 补充 filter-changed-test-files 的断言用例。
(check-true (procedure? filter-changed-test-files))

(check-report)
