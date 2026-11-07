;; clang-format-ok?
;; TODO: 补充 clang-format-ok? 的简要描述。
;;
;; 语法
;; ----
;; (clang-format-ok? ...)
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

;; TODO: 补充 clang-format-ok? 的断言用例。
(check-true (procedure? clang-format-ok?))

(check-report)
