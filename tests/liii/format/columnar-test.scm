(import (liii check)
        (liii format base)
        (liii format columnar))

(check-set-mode! 'report-failed)

;; (liii format columnar) 模块函数分类索引
;;
;; columnar 模块提供多列对齐排版、自动折行、表格和行号组合子。
;;
;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/format/columnar "wrapped"
;;   bin/gf doc liii/format/columnar "columnar"

(check (show #f (with ((width 8)) (wrapped "hello world"))) => "hello\nworld")
(check (show #f (columnar (each "*"))) => "*\n")

(check-report)
