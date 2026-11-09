;; pretty-with-color
;; 带 ANSI 着色的高亮排版组合子。
;;
;; 语法
;; ----
;; (pretty-with-color obj)
;;
;; 参数
;; ----
;; obj : any
;; 待排版的对象。
;;
;; 返回值
;; ----
;; format?
;; 输出带颜色转义序列代码文本的组合子。
;;
;; 说明
;; ----
;; 排版规则与 pretty 相同，另为 define、let 等关键字加上
;; 前景色；颜色码不占显示宽度，需要按列对齐时请配合
;; terminal-aware 使用。
;; `(liii format pretty)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 pretty))

(check-set-mode! 'report-failed)

(check (show #f (pretty-with-color '(foo bar))) => "(foo bar)\n")
(check
  (show #f (pretty-with-color '(define (f x) x)))
  =>
  "(\x1b;[34mdefine\x1b;[39m (f x) x)\n"
) ;check
(check
  (show #f (pretty-with-color '(let ((x 1)) x)))
  =>
  "(\x1b;[34mlet\x1b;[39m ((x 1)) x)\n"
) ;check

(check-report)
