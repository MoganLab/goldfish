(import (liii check)
        (srfi srfi-166 base))

(check-set-mode! 'report-failed)

;; trimmed
;; 截断组合子，将超出宽度的内容裁剪，并支持添加省略号 (ellipsis)。
;;
;; 语法
;; ----
;; (trimmed width . ls)
;; (trimmed/right width . ls)
;; (trimmed/both width . ls)
;; (trimmed/lazy width . ls)
;;
;; 示例
;; ----
;; (show #f (trimmed 3 "abcde")) => "cde"
;; (show #f (trimmed/right 3 "abcde")) => "abc"
;; (show #f (trimmed/both 3 "abcde")) => "bcd"
;;
;; 另见
;; ----
;; (liii format base) 同名再导出本函数，日常使用更推荐导入该库：
;;   (import (liii format base))
;;   (show #f (trimmed 3 "abcde"))

(check (show #f (trimmed 3 "abcde")) => "cde")
(check (show #f (trimmed/right 3 "abcde")) => "abc")
(check (show #f (trimmed/both 3 "abcde")) => "bcd")
(check (show #f (with ((ellipsis "...")) (trimmed/right 5 "abcdef"))) => "ab...")
(check (show #f (trimmed/lazy 3 "abcde")) => "abc")

(check-report)
