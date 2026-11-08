;; on-true-color
;; 24-bit 真彩色背景色格式化组合子（增强版）。
;;
;; 语法
;; ----
;; (on-true-color '(r g b) fmt ...)
;; (on-true-color "#rrggbb" fmt ...)
;; (on-true-color r g b fmt ...)
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
;; 包裹了 ANSI 24-bit 背景控制序列的格式化组合子。
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

(import (liii check) (liii format base) (liii format color))

(check-set-mode! 'report-failed)

;; RGB 列表形式
(check (show #f (on-true-color '(10 20 30) "bg"))
  =>
  "\x1b;[48;2;10;20;30mbg\x1b;[49m"
) ;check

;; hex 字符串形式
(check (show #f (on-true-color "#0a141e" "bg"))
  =>
  "\x1b;[48;2;10;20;30mbg\x1b;[49m"
) ;check

;; 错误用例
(check-catch 'type-error (on-true-color 10 20 "bg"))
(check-catch 'type-error (on-true-color '(10 20) "bg"))
(check-catch 'type-error (on-true-color '(10 20 256) "bg"))
(check-catch 'type-error (on-true-color "#zzggbb" "bg"))
(check-catch 'type-error (on-true-color "#0a141" "bg"))

(check-report)
