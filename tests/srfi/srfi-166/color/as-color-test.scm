(import (liii check)
        (srfi srfi-166 base)
        (srfi srfi-166 color))

(check-set-mode! 'report-failed)

;; as-color
;; 终端自定义 RGB 颜色格式化组合子。as-color 支持 6x6x6 调色板（0~5 级别）。
;; as-true-color/on-true-color 的文档和用例见：
;;   gf doc liii/format/color as-true-color
;;
;; 语法
;; ----
;; (as-color r g b fmt ...)
;; (as-true-color r g b fmt ...)
;; (on-color r g b fmt ...)
;; (on-true-color r g b fmt ...)
;;
;; 示例
;; ----
;; (show #f (as-color 5 0 0 "red")) => "\x1b[38;5;196mred\x1b[39m"
;;
;; 另见
;; ----
;; (liii format color) 同名再导出本函数，日常使用更推荐导入该库：
;;   (import (liii format color))
;;   (show #f (as-color 5 0 0 "red"))

(check (show #f (as-color 5 0 0 "red")) => "\x1b[38;5;196mred\x1b[39m")
(check (show #f (on-color 0 5 0 "green-bg")) => "\x1b[48;5;46mgreen-bg\x1b[49m")
(check (show #f (as-true-color 255 128 0 "orange"))
       => "\x1b[38;2;255;128;0morange\x1b[39m")
(check (show #f (on-true-color 10 20 30 "dark-bg"))
       => "\x1b[48;2;10;20;30mdark-bg\x1b[49m")

(check-report)
