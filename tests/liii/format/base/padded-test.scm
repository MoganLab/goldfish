(import (liii check)
        (liii format base))

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

(check (show #f (padded 5 "abc")) => "  abc")
(check (show #f (padded/right 5 "abc")) => "abc  ")
(check (show #f (padded/both 5 "abc")) => " abc ")
(check (show #f (padded 3 "abcde")) => "abcde")

(check-report)
