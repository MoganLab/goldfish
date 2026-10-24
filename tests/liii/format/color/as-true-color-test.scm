(import (liii check)
        (liii format base)
        (liii format color))

(check-set-mode! 'report-failed)

;; as-true-color
;; as-true-color/on-true-color 的增强版：颜色可以作为单个值传递。
;; 完全兼容 srfi-166 的散参形式 (as-true-color r g b fmt ...)，散参形式的
;; 单元测试见：
;;   gf doc srfi/srfi-166/color as-true-color
;;
;; 语法
;; ----
;; (as-true-color '(r g b) fmt ...)
;; (as-true-color "#rrggbb" fmt ...)
;; (on-true-color '(r g b) fmt ...)
;; (on-true-color "#rrggbb" fmt ...)
;; r、g、b 为 0~255 的精确整数；hex 字符串的 # 前缀可省略，大小写均可。
;;
;; 示例
;; ----
;; (show #f (as-true-color '(138 190 183) "gf"))
;;   => "\x1b[38;2;138;190;183mgf\x1b[39m"
;; (show #f (as-true-color "#8abeb7" "gf"))
;;   => "\x1b[38;2;138;190;183mgf\x1b[39m"

;; RGB 列表形式
(check (show #f (as-true-color '(138 190 183) "gf"))
       => "\x1b[38;2;138;190;183mgf\x1b[39m")
(check (show #f (as-true-color '(138 190 183) "g" "f"))
       => "\x1b[38;2;138;190;183mgf\x1b[39m")

;; hex 字符串形式：带 # / 不带 # / 大写
(check (show #f (as-true-color "#8abeb7" "gf"))
       => "\x1b[38;2;138;190;183mgf\x1b[39m")
(check (show #f (as-true-color "8abeb7" "gf"))
       => "\x1b[38;2;138;190;183mgf\x1b[39m")
(check (show #f (as-true-color "#8ABEB7" "gf"))
       => "\x1b[38;2;138;190;183mgf\x1b[39m")

;; on-true-color 同样支持列表和 hex
(check (show #f (on-true-color '(10 20 30) "bg"))
       => "\x1b[48;2;10;20;30mbg\x1b[49m")
(check (show #f (on-true-color "#0a141e" "bg"))
       => "\x1b[48;2;10;20;30mbg\x1b[49m")

;; 错误用例
;; 散参不完整：r g b 必须齐全
(check-catch 'type-error (as-true-color 138 190 "gf"))
;; RGB 列表长度不是 3
(check-catch 'type-error (as-true-color '(138 190) "gf"))
;; RGB 分量超出 0~255
(check-catch 'type-error (as-true-color '(138 190 256) "gf"))
;; 非法 hex 字符
(check-catch 'type-error (as-true-color "#zzggbb" "gf"))
;; hex 位数不是 6
(check-catch 'type-error (as-true-color "#8abeb" "gf"))

(check-report)
