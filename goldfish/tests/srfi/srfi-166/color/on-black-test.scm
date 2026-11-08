;; on-black
;; 终端背景色格式化组合子。将输出内容包裹在 ANSI 背景色 40 控制序列中，并在结束时恢复之前的背景色。
;;
;; 语法
;; ----
;; (on-black fmt ...)
;;
;; 参数
;; ----
;; fmt : format?
;; 零个或多个格式化对象（字符串或其他格式化组合子）。
;;
;; 返回值
;; ----
;; format?
;; 包裹了 ANSI 背景色 40（黑色）控制序列的格式化组合子。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 color))

(check-set-mode! 'report-failed)

(check (show #f (on-black "black-bg")) => "\x1b;[40mblack-bg\x1b;[49m")
(check (show #f (on-black "a" "b")) => "\x1b;[40mab\x1b;[49m")
(check (show #f (on-black)) => "\x1b;[40m\x1b;[49m")

(check-report)
