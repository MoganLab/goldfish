;; fitted/right
;; 右对齐内容的定宽组合子：左侧填充、右侧裁剪。
;;
;; 语法
;; ----
;; (fitted/right width . ls)
;;
;; 参数
;; ----
;; width : exact-integer?
;; 目标显示宽度。
;;
;; ls : format?
;; 零个或多个格式化对象。
;;
;; 返回值
;; ----
;; format?
;; 输出恰好占 width 列的格式化组合子。
;;
;; 说明
;; ----
;; 与 fitted 相反：fitted/right 内容靠左，不足处在右侧补空白，
;; 超出时从右侧裁掉。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (fitted/right 6 "abc")) => "abc   ")
(check (show #f (fitted/right 6 "abcdefgh")) => "abcdef")
(check
  (show #f (with ((pad-char #\~)) (fitted/right 5 "abc")))
  =>
  "abc~~"
) ;check

(check-report)
