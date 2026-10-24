(import (liii check)
        (srfi srfi-166 base)
        (srfi srfi-166 color))

(check-set-mode! 'report-failed)

;; as-true-color
;; 24-bit 真彩色格式化组合子（srfi-166 散参形式）。
;; as-true-color 设置前景色，on-true-color 设置背景色。
;;
;; 语法
;; ----
;; (as-true-color r g b fmt ...)
;; (on-true-color r g b fmt ...)
;; r、g、b 为 0~255 的精确整数。
;;
;; 示例
;; ----
;; (show #f (as-true-color 138 190 183 "gf")) => "\x1b[38;2;138;190;183mgf\x1b[39m"
;;
;; 另见
;; ----
;; (liii format color) 提供了更方便的形式，还支持 RGB 列表和 hex 字符串：
;;   gf doc liii/format/color as-true-color

(check (show #f (as-true-color 138 190 183 "gf"))
       => "\x1b[38;2;138;190;183mgf\x1b[39m")
(check (show #f (as-true-color 255 128 0 "or" "ange"))
       => "\x1b[38;2;255;128;0morange\x1b[39m")
(check (show #f (as-true-color 138 190 183))
       => "\x1b[38;2;138;190;183m\x1b[39m")
(check (show #f (on-true-color 10 20 30 "dark-bg"))
       => "\x1b[48;2;10;20;30mdark-bg\x1b[49m")

(check-report)
