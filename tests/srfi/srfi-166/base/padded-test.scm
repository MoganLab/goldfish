(import (liii check)
        (srfi srfi-166 base))

(check-set-mode! 'report-failed)

;; padded
;; 填充组合子，将内容对齐填充至指定宽度。
;;
;; 语法
;; ----
;; (padded width . ls)
;; (padded/right width . ls)
;; (padded/both width . ls)
;;
;; 示例
;; ----
;; (show #f (padded 5 "abc")) => "  abc"
;; (show #f (padded/right 5 "abc")) => "abc  "
;; (show #f (padded/both 5 "abc")) => " abc "
;;
;; 另见
;; ----
;; (liii format base) 同名再导出本函数，日常使用更推荐导入该库：
;;   (import (liii format base))
;;   (show #f (padded 5 "abc"))

(check (show #f (padded 5 "abc")) => "  abc")
(check (show #f (padded/right 5 "abc")) => "abc  ")
(check (show #f (padded/both 5 "abc")) => " abc ")
(check (show #f (padded 3 "abcde")) => "abcde")

(check-report)
