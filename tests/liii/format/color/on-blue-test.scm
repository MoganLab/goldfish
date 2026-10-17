(import (liii check)
        (liii format base)
        (liii format color))

(check-set-mode! 'report-failed)

;; on-blue
;; 终端背景色格式化组合子。将输出内容包裹在 ANSI 背景颜色控制序列中，并在结束时恢复之前的背景色。
;;
;; 语法
;; ----
;; (on-red fmt ...)
;; (on-blue fmt ...)
;; (on-green fmt ...)
;; (on-cyan fmt ...)
;; (on-yellow fmt ...)
;; (on-magenta fmt ...)
;; (on-white fmt ...)
;; (on-black fmt ...)
;;
;; 示例
;; ----
;; (show #f (on-blue "blue-bg")) => "\x1b[44mblue-bg\x1b[49m"

(check (show #f (on-blue "blue-bg")) => "\x1b[44mblue-bg\x1b[49m")
(check (show #f (on-red "red-bg")) => "\x1b[41mred-bg\x1b[49m")
(check (show #f (on-green "green-bg")) => "\x1b[42mgreen-bg\x1b[49m")
(check (show #f (on-yellow "yellow-bg")) => "\x1b[43myellow-bg\x1b[49m")
(check (show #f (on-cyan "cyan-bg")) => "\x1b[46mcyan-bg\x1b[49m")
(check (show #f (on-magenta "magenta-bg")) => "\x1b[45mmagenta-bg\x1b[49m")
(check (show #f (on-white "white-bg")) => "\x1b[47mwhite-bg\x1b[49m")
(check (show #f (on-black "black-bg")) => "\x1b[40mblack-bg\x1b[49m")
(check (show #f (on-blue (each (as-yellow "abc") "def")))
       => "\x1b[44m\x1b[33mabc\x1b[39mdef\x1b[49m")

(check-report)
