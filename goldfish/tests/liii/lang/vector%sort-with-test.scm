;; vector%sort-with
;; 向量按比较器排序（对标 Scala Vector.sortWith）。
;;
;; 语法
;; ----
;; ($ v :sort-with cmp)
;;
;; 参数
;; ----
;; cmp : procedure?
;; 比较函数，如 <。
;;
;; v : vector?
;; 源向量。
;;
;; 返回值
;; ------
;; vector?
;; 排序后的新向量，不改原向量。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(2 1) :sort-with <) => #(1 2))
(check ($ #() :sort-with <) => #())
(check ($ #(9 3 7) :sort-with >) => #(9 7 3))

(check-report)
