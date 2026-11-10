;; on-color
;; 终端自定义颜色格式化组合子，按 6x6x6 调色板（各级别 0~5）设置背景色。
;;
;; 语法
;; ----
;; (on-color r g b fmt ...)
;;
;; 参数
;; ----
;; r : exact-integer?
;; 红色级别，0~5。
;;
;; g : exact-integer?
;; 绿色级别，0~5。
;;
;; b : exact-integer?
;; 蓝色级别，0~5。
;;
;; fmt : format?
;; 零个或多个格式化对象。
;;
;; 返回值
;; ----
;; format?
;; 包裹了 ANSI 256 色背景控制序列的格式化组合子，颜色号为 16 + 36*r + 6*g + b。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 color))

(check-set-mode! 'report-failed)

(check (show #f (on-color 0 5 0 "green-bg"))
  =>
  "\x1b;[48;5;46mgreen-bg\x1b;[49m"
) ;check
(check (show #f (on-color 0 0 0 "black-bg"))
  =>
  "\x1b;[48;5;16mblack-bg\x1b;[49m"
) ;check

(check-report)
