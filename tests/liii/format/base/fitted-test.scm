(import (liii check)
        (liii format base))

(check-set-mode! 'report-failed)

;; fitted
;; 定宽组合子，将内容裁剪或填充至精确的宽度。
;;
;; 语法
;; ----
;; (fitted width . ls)
;; (fitted/right width . ls)
;; (fitted/both width . ls)
;;
;; 示例
;; ----
;; (show #f (fitted 5 "abc")) => "  abc"
;; (show #f (fitted/right 5 "abc")) => "abc  "
;; (show #f (fitted 5 "abcdefgh")) => "defgh"

(check (show #f (fitted 5 "abc")) => "  abc")
(check (show #f (fitted/right 5 "abc")) => "abc  ")
(check (show #f (fitted 5 "abcdefgh")) => "defgh")
(check (show #f (fitted/both 5 "abc")) => " abc ")

(check-report)
