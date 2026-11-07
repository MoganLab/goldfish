;; clang-format-run
;; TODO: 补充 clang-format-run 的简要描述。
;;
;; 语法
;; ----
;; (clang-format-run ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt cpp-runner))

(check-set-mode! 'report-failed)

;; TODO: 补充 clang-format-run 的断言用例。
(check-true (procedure? clang-format-run))

(check-report)
