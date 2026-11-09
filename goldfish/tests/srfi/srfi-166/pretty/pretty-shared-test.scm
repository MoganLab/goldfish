;; pretty-shared
;; 美化输出 S 表达式并标注共享结构的组合子。
;;
;; 语法
;; ----
;; (pretty-shared obj)
;;
;; 参数
;; ----
;; obj : any
;; 待排版的对象。
;;
;; 返回值
;; ----
;; format?
;; 输出带 `#n=` / `#n#` 标签代码文本的组合子。
;;
;; 说明
;; ----
;; 排版规则与 pretty 相同，差别只在共享与循环结构的标注，
;; 需要完全无标签的输出请用 pretty-simply。
;; `(liii format pretty)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 pretty))

(check-set-mode! 'report-failed)

(check (show #f (pretty-shared '(1 2 3))) => "(1 2 3)\n")
(check
  (show #f
    (pretty-shared
      (let ((ones (list 1)))
        (set-cdr! ones ones)
        (list 'let (list (list 'ones (list 'quote ones))) 'ones)
      ) ;let
    ) ;pretty-shared
  ) ;show
  =>
  "(let ((ones '#0=(1 . #0#))) ones)\n"
) ;check

(check-report)
