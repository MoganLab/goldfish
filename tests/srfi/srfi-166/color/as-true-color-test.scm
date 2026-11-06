;; as-true-color
;; 24-bit 真彩色格式化组合子，按 RGB 分量设置前景色。
;;
;; 语法
;; ----
;; (as-true-color r g b fmt ...)
;;
;; 参数
;; ----
;; r : exact-integer?
;; 红色分量，0~255。
;;
;; g : exact-integer?
;; 绿色分量，0~255。
;;
;; b : exact-integer?
;; 蓝色分量，0~255。
;;
;; fmt : format?
;; 零个或多个格式化对象。
;;
;; 返回值
;; ----
;; format?
;; 包裹了 ANSI 24-bit 前景控制序列的格式化组合子。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 color))

(check-set-mode! 'report-failed)

(check (show #f (as-true-color 138 190 183 "gf"))
  =>
  "\x1b;[38;2;138;190;183mgf\x1b;[39m"
) ;check
(check (show #f (as-true-color 255 128 0 "or" "ange"))
  =>
  "\x1b;[38;2;255;128;0morange\x1b;[39m"
) ;check
(check (show #f (as-true-color 138 190 183))
  =>
  "\x1b;[38;2;138;190;183m\x1b;[39m"
) ;check

(check-report)
