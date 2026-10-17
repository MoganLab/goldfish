(import (liii check)
        (liii format base)
        (liii format color))

(check-set-mode! 'report-failed)

;; as-red
;; 终端前景色格式化组合子。将输出内容包裹在 ANSI 前景颜色控制序列中，并在结束时恢复之前的颜色。
;;
;; 语法
;; ----
;; (as-red fmt ...)
;; (as-blue fmt ...)
;; (as-green fmt ...)
;; (as-cyan fmt ...)
;; (as-yellow fmt ...)
;; (as-magenta fmt ...)
;; (as-white fmt ...)
;; (as-black fmt ...)
;;
;; 示例
;; ----
;; (show #f (as-red "red")) => "\x1b[31mred\x1b[39m"

(check (show #f (as-red "red")) => "\x1b[31mred\x1b[39m")
(check (show #f (as-red "red" (as-blue "blue") "red"))
       => "\x1b[31mred\x1b[34mblue\x1b[31mred\x1b[39m")
(check (show #f (as-green "green")) => "\x1b[32mgreen\x1b[39m")
(check (show #f (as-yellow "yellow")) => "\x1b[33myellow\x1b[39m")
(check (show #f (as-cyan "cyan")) => "\x1b[36mcyan\x1b[39m")
(check (show #f (as-magenta "magenta")) => "\x1b[35mmagenta\x1b[39m")
(check (show #f (as-white "white")) => "\x1b[37mwhite\x1b[39m")
(check (show #f (as-black "black")) => "\x1b[30mblack\x1b[39m")

(check-report)
