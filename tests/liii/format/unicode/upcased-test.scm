(import (liii check)
        (liii format base)
        (liii format unicode))

(check-set-mode! 'report-failed)

;; upcased
;; 字符串大小写转换组合子。
;;
;; 语法
;; ----
;; (upcased fmt ...)
;; (downcased fmt ...)
;;
;; 示例
;; ----
;; (show #f (upcased "abc")) => "ABC"
;; (show #f (downcased "ABC")) => "abc"

(check (show #f (upcased "abc")) => "ABC")
(check (show #f (downcased "ABC")) => "abc")
(check (show #f (upcased "hello") " " (downcased "WORLD")) => "HELLO world")

(check-report)
