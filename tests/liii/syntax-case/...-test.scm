;; ...
;; 辅助语法关键字，在宏模板中作为省略号重复指示符。
;;
;; 语法
;; ----
;; (...)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; -----
;; 无。直接调用时抛出 syntax-error 异常。

(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; 1. 独立求值抛出 syntax-error
(check-catch 'syntax-error
  (...))

(check-report)
