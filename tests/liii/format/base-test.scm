(import (liii check)
        (liii format base))

(check-set-mode! 'report-failed)

;; (liii format base) 模块函数分类索引
;;
;; base 提供 SRFI-166 Monadic Formatting 的核心运行机制与基础排版组合子。
;;
;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/format/base "show"
;;   bin/gf doc liii/format/base "numeric"
;;   bin/gf doc liii/format/base "padded"
;;   bin/gf doc liii/format/base "trimmed"
;;   bin/gf doc liii/format/base "joined"

(check (show #f "hello " "world") => "hello world")
(check (show #f (padded 6 "abc")) => "   abc")
(check (show #f (trimmed 3 "abcde")) => "cde")
(check (show #f (numeric 100)) => "100")

(check-report)
