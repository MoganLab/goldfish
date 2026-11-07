;; find-tool-readme
;; TODO: 补充 find-tool-readme 的简要描述。
;;
;; 语法
;; ----
;; (find-tool-readme ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldhelp))

(check-set-mode! 'report-failed)

;; TODO: 补充 find-tool-readme 的断言用例。
(check-true (procedure? find-tool-readme))

(check-report)
