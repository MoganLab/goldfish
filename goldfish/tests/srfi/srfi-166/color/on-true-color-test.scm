;; on-true-color
;; 24-bit 真彩色格式化组合子，按 RGB 分量设置背景色。
;;
;; 语法
;; ----
;; (on-true-color r g b fmt ...)
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
;; 包裹了 ANSI 24-bit 背景控制序列的格式化组合子。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 color))

(check-set-mode! 'report-failed)

(check (show #f (on-true-color 10 20 30 "dark-bg"))
  =>
  "\x1b;[48;2;10;20;30mdark-bg\x1b;[49m"
) ;check
(check (show #f (on-true-color 255 255 255 "white-bg"))
  =>
  "\x1b;[48;2;255;255;255mwhite-bg\x1b;[49m"
) ;check

(check-report)
