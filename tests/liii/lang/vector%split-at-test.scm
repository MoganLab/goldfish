;; vector%split-at
;; 在下标 n 处将向量二分（对标 Scala Vector.splitAt）。
;;
;; 语法
;; ----
;; ($ v :split-at n)
;;
;; 参数
;; ----
;; n : integer?
;; 切分位置。
;;
;; v : vector?
;; 源向量。
;;
;; 返回值
;; ------
;; list?
;; (#(take n) #(drop n)) 二元组。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2 3) :split-at 1) => '(#(1) #(2 3)))
(check ($ #() :split-at 2) => '(#() #()))
(check ($ #(1 2) :split-at 10) => '(#(1 2) #()))

(check-report)
