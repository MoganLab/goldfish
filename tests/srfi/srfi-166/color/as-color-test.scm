;; as-color
;; 终端自定义颜色格式化组合子，按 6x6x6 调色板（各级别 0~5）设置前景色。
;;
;; 语法
;; ----
;; (as-color r g b fmt ...)
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
;; 包裹了 ANSI 256 色前景控制序列的格式化组合子，颜色号为 16 + 36*r + 6*g + b。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 color))

(check-set-mode! 'report-failed)

(check (show #f (as-color 5 0 0 "red")) => "\x1b;[38;5;196mred\x1b;[39m")
(check (show #f (as-color 0 0 0 "black")) => "\x1b;[38;5;16mblack\x1b;[39m")
(check (show #f (as-color 5 5 5 "white")) => "\x1b;[38;5;231mwhite\x1b;[39m")

(check-report)
