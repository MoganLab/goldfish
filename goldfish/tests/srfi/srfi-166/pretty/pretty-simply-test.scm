;; pretty-simply
;; 美化输出 S 表达式且不标注共享结构的组合子。
;;
;; 语法
;; ----
;; (pretty-simply obj)
;;
;; 参数
;; ----
;; obj : any
;; 待排版的对象。
;;
;; 返回值
;; ----
;; format?
;; 输出纯净代码文本的组合子。
;;
;; 说明
;; ----
;; 不生成任何 `#n=` / `#n#` 标签，共享子对象会被重复展开，
;; 因此循环结构会导致无限递归。
;; `(liii format pretty)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 pretty))

(check-set-mode! 'report-failed)

(check (show #f (pretty-simply '(foo bar))) => "(foo bar)\n")
(check
  (show #f (pretty-simply (let ((x (list 1))) (vector x x))))
  =>
  "#((1)\n  (1))\n"
) ;check

(check-report)
