;; vector%reverse
;; 反转向量（对标 Scala Vector.reverse）。
;;
;; 语法
;; ----
;; ($ v :reverse)
;;
;; 参数
;; ----
;; v : vector?
;; 源向量。
;;
;; 返回值
;; ------
;; vector?
;; 元素顺序完全颠倒的新向量；不修改原向量。
;;
;; 描述
;; ----
;; vector 路径去糖为 (vector-reverse-copy v)（srfi-133）。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2 3) :reverse) => #(3 2 1))
(check ($ #() :reverse) => #())
(check ($ #(1 2 3) :reverse :reverse) => #(1 2 3))

(check-report)
