(import (liii check)
        (srfi srfi-166 base))

(check-set-mode! 'report-failed)

;; (srfi srfi-166 base) 模块函数分类索引
;;
;; base 提供 SRFI-166 Monadic Formatting 的核心运行机制与基础排版组合子。
;;
;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc srfi/srfi-166/base "show"
;;   bin/gf doc srfi/srfi-166/base "numeric"
;;   bin/gf doc srfi/srfi-166/base "padded"
;;   bin/gf doc srfi/srfi-166/base "trimmed"
;;   bin/gf doc srfi/srfi-166/base "joined"

(check (show #f "hello " "world") => "hello world")
(check (show #f (padded 6 "abc")) => "   abc")
(check (show #f (trimmed 3 "abcde")) => "cde")
(check (show #f (numeric 100)) => "100")

(check-report)
