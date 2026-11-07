;; main
;; TODO: 补充 main 的简要描述。
;;
;; 语法
;; ----
;; (main ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii golddoc))

(check-set-mode! 'report-failed)

;; TODO: 补充 main 的断言用例。
(check-true (procedure? main))

(check-report)
