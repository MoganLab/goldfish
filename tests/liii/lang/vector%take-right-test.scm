;; vector%take-right
;; 取向量尾部 n 个元素，越界宽容（对标 Scala Vector.takeRight）。
;;
;; 语法
;; ----
;; ($ v :take-right n)
;;
;; 参数
;; ----
;; n : integer?
;; 保留的尾部元素数，越界/负数宽容。
;;
;; v : vector?
;; 源向量。
;;
;; 返回值
;; ------
;; vector?
;; 尾部 n 个元素的新向量。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2 3) :take-right 2) => #(2 3))
(check ($ #(1 2) :take-right 5) => #(1 2))
(check ($ #() :take-right 3) => #())

(check-report)
