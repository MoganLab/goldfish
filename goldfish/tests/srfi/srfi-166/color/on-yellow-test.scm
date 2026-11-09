;; on-yellow
;; 终端背景色格式化组合子。将输出内容包裹在 ANSI 背景色 43 控制序列中，并在结束时恢复之前的背景色。
;;
;; 语法
;; ----
;; (on-yellow fmt ...)
;;
;; 参数
;; ----
;; fmt : format?
;; 零个或多个格式化对象（字符串或其他格式化组合子）。
;;
;; 返回值
;; ----
;; format?
;; 包裹了 ANSI 背景色 43（黄色）控制序列的格式化组合子。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 color))

(check-set-mode! 'report-failed)

(check (show #f (on-yellow "yellow-bg")) => "\x1b;[43myellow-bg\x1b;[49m")
(check (show #f (on-yellow "a" "b")) => "\x1b;[43mab\x1b;[49m")
(check (show #f (on-yellow)) => "\x1b;[43m\x1b;[49m")

(check-report)
