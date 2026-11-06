;; padded/right
;; 右侧填充组合子：内容不足宽度时在尾部补空白。
;;
;; 语法
;; ----
;; (padded/right width . ls)
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
;; 输出至少占 width 列、内容左对齐的格式化组合子。
;;
;; 说明
;; ----
;; 填充字符取状态变量 pad-char；内容已超出宽度时原样输出。
;; 左侧填充请用 padded，两侧填充请用 padded/both。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (padded/right 6 "abc")) => "abc   ")
(check (show #f (padded/right 6 "abcdefg")) => "abcdefg")
(check
  (show #f (with ((pad-char #\~)) (padded/right 5 "abc")))
  =>
  "abc~~"
) ;check

(check-report)
