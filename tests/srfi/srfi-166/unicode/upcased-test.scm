(import (liii check)
        (srfi srfi-166 base)
        (srfi srfi-166 unicode))

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
;;
;; 另见
;; ----
;; (liii format unicode) 同名再导出本函数，日常使用更推荐导入该库：
;;   (import (liii format unicode))
;;   (show #f (upcased "abc"))

(check (show #f (upcased "abc")) => "ABC")
(check (show #f (downcased "ABC")) => "abc")
(check (show #f (upcased "hello") " " (downcased "WORLD")) => "HELLO world")

(check-report)
