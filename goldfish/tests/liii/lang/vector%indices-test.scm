;; vector%indices
;; 获取向量从 0 开始的全部合法索引向量（对标 Scala Vector.indices）。
;;
;; 语法
;; ----
;; ($ v :indices)
;;
;; 参数
;; ----
;; v : vector?
;; 源向量。
;;
;; 返回值
;; ------
;; vector?
;; #(0 1 ... len-1)；空向量返回 #()。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(10 20) :indices) => '#(0 1))
(check ($ #() :indices) => '#())

(check-report)
