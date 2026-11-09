;; vector%zip-with-index
;; 将向量中每个元素与从 0 开始的下标配对（对标 Scala Vector.zipWithIndex）。
;;
;; 语法
;; ----
;; ($ v :zip-with-index)
;;
;; 参数
;; ----
;; v : vector?
;; 源向量。
;;
;; 返回值
;; ------
;; vector?
;; #(#(elem 0) #(elem 1) ...) 二元组向量；空向量返回 #()。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(10 20) :zip-with-index) => '#(#(10 0) #(20 1)))
(check ($ #() :zip-with-index) => #())

(check-report)
