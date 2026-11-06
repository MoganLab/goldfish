;; as-true-color
;; 24-bit 真彩色前景色格式化组合子（增强版）。
;;
;; 语法
;; ----
;; (as-true-color '(r g b) fmt ...)
;; (as-true-color "#rrggbb" fmt ...)
;; (as-true-color r g b fmt ...)
;;
;; 参数
;; ----
;; color : list 或 string
;; RGB 列表 '(r g b)（各分量 0~255）或 hex 字符串 "#rrggbb"（# 前缀可省略，大小写均可）。
;; r : exact-integer?
;; 散参形式的红色分量，0~255。
;; g : exact-integer?
;; 散参形式的绿色分量，0~255。
;; b : exact-integer?
;; 散参形式的蓝色分量，0~255。
;; fmt : any
;; 零个或多个格式化对象。
;;
;; 返回值
;; ----
;; format?
;; 包裹了 ANSI 24-bit 前景控制序列的格式化组合子。
;;
;; 说明
;; ----
;; 1. 本库在 srfi-166 的散参形式之上增强：颜色可作为单个值传递。
;; 2. srfi-166 散参形式的单元测试见 tests/srfi/srfi-166/color/ 目录。
;;
;; 错误处理
;; ----
;; type-error
;; RGB 列表长度不为 3、分量超出 0~255、hex 字符串非法时抛出。

(import (liii check)
        (liii format base)
        (liii format color))

(check-set-mode! 'report-failed)

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
