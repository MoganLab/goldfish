;; collect-files-dfs
;; TODO: 补充 collect-files-dfs 的简要描述。
;;
;; 语法
;; ----
;; (collect-files-dfs ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt path))

(check-set-mode! 'report-failed)

;; TODO: 补充 collect-files-dfs 的断言用例。
(check-true (procedure? collect-files-dfs))

(check-report)
