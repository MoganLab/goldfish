(import (liii check)
        (srfi srfi-166 base)
        (srfi srfi-166 pretty))

(check-set-mode! 'report-failed)

;; (srfi srfi-166 pretty) 模块函数分类索引
;;
;; pretty 模块提供 S 表达式代码的美化排版打印组合子。
;;
;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc srfi/srfi-166/pretty "pretty"

(check (show #f (pretty '(foo bar))) => "(foo bar)\n")

(check-report)
