;; snake-compute-dimensions
;; 按终端尺寸计算游戏区尺寸（宽高二元组）
;;
;; 语法
;; ----
;; (snake-compute-dimensions term-w term-h)
;;
;; 参数
;; ----
;; term-w, term-h : integer?\n终端列数与行数
;;
;; 返回值
;; ------
;; list?\n(width height) 游戏区尺寸二元组

(import (scheme base)
  (liii check)
  (liii tui snake view)
) ;import

(check-set-mode! 'report-failed)

(check (snake-compute-dimensions 80 24) => '(50 22))
(check (snake-compute-dimensions 120 40) => '(50 25))
(check (snake-compute-dimensions 48 20) => '(27 18))
(check (snake-compute-dimensions 30 20) => '(28 11))

(check-report)
