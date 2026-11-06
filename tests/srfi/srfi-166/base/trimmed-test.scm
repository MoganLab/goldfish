;; trimmed
;; 截断组合子：把内容裁剪到指定宽度，可加省略号。
;;
;; 语法
;; ----
;; (trimmed width . ls)
;; (trimmed/right width . ls)
;; (trimmed/both width . ls)
;; (trimmed/lazy width . ls)
;;
;; 参数
;; ----
;; width : exact-integer?
;; 允许的最大显示宽度（含省略号）。
;;
;; ls : format?
;; 零个或多个格式化对象。
;;
;; 返回值
;; ----
;; format?
;; 输出不超过 width 列内容的格式化组合子。
;;
;; 说明
;; ----
;; 状态变量 ellipsis 非空时，被裁掉的一侧会用省略号补位；
;; trimmed 保留尾部，trimmed/right 保留头部，trimmed/both
;; 保留中间，trimmed/lazy 直接丢弃超出部分而不计算内容。
;; `(liii format base)` 以 Goldfish 命名重导出这些函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (trimmed 3 "abcde")) => "cde")
(check (show #f (trimmed 10 "abc")) => "abc")
(check
  (show #f (with ((ellipsis "...")) (trimmed 5 "abcdef")))
  =>
  "...ef"
) ;check

(check-report)
