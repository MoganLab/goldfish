(import (liii check)
        (srfi srfi-166 base)
        (srfi srfi-166 columnar))

(check-set-mode! 'report-failed)

;; wrapped
;; 自动折行格式化组合子。根据当前 width 限制自动折行单词序列。
;;
;; 语法
;; ----
;; (wrapped fmt ...)
;; (wrapped/char fmt ...)
;; (justified fmt ...)
;;
;; 示例
;; ----
;; (show #f (with ((width 8)) (wrapped "hello world"))) => "hello\nworld"
;;
;; 另见
;; ----
;; (liii format columnar) 同名再导出本函数，日常使用更推荐导入该库：
;;   (import (liii format columnar))
;;   (show #f (with ((width 8)) (wrapped "hello world")))

(check (show #f (wrapped "foo")) => "foo")
(check (show #f (wrapped "    ")) => "")
(check (show #f (with ((width 8)) (wrapped "hello world"))) => "hello\nworld")

(check (show #f
             (with ((width 10))
               (justified "The quick brown fox jumped over the lazy dog")))
       => "The  quick\nbrown  fox\njumped\nover   the\nlazy dog\n")

(check-report)
