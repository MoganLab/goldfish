;; padded
;; 填充组合子：内容不足宽度时按对齐方向补齐。
;;
;; 语法
;; ----
;; (padded width . ls)
;; (padded/right width . ls)
;; (padded/both width . ls)
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
;; 输出至少占 width 列的格式化组合子。
;;
;; 说明
;; ----
;; 填充字符取状态变量 pad-char（默认空格）；内容已超出宽度时
;; 原样输出，不裁剪，需要裁剪请改用 fitted 或 trimmed。
;; `(liii format base)` 以 Goldfish 命名重导出这些函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (padded 5 "abc")) => "  abc")
(check (show #f (padded 3 "abcde")) => "abcde")
(check (show #f (padded 5 "")) => "     ")
(check
  (show #f (with ((pad-char #\~)) (padded 5 "abc")))
  =>
  "~~abc"
) ;check

(check-report)
