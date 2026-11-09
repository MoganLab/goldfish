;; vector%drop-right
;; 去掉向量尾部 n 个元素，越界宽容（对标 Scala Vector.dropRight）。
;;
;; 语法
;; ----
;; ($ v :drop-right n)
;;
;; 参数
;; ----
;; n : integer?
;; 去掉的尾部元素数，越界/负数宽容。
;;
;; v : vector?
;; 源向量。
;;
;; 返回值
;; ------
;; vector?
;; 去掉尾部 n 个后的新向量。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2 3) :drop-right 2) => #(1))
(check ($ #(1 2) :drop-right -1) => #(1 2))
(check ($ #(1 2) :drop-right 10) => #())

(check-report)
