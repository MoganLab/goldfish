;; justified
;; 两端对齐组合子：拉伸词间空白使每行铺满宽度。
;;
;; 语法
;; ----
;; (justified . ls)
;;
;; 参数
;; ----
;; ls : format?
;; 零个或多个格式化对象。
;;
;; 返回值
;; ----
;; format?
;; 每行按 width 两端对齐输出的格式化组合子。
;;
;; 说明
;; ----
;; 行末不做拉伸，最后一行保持自然宽度；单词宽度不足时
;; 允许短于 width。
;; `(liii format columnar)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 columnar))

(check-set-mode! 'report-failed)

(check
  (show #f
    (with ((width 10)) (justified "The quick brown fox jumped over the lazy dog"))
  ) ;show
  =>
  "The  quick\nbrown  fox\njumped\nover   the\nlazy dog\n"
) ;check
(check
  (show #f (with ((width 10)) (justified "ab")))
  =>
  "ab\n"
) ;check

(check-report)
