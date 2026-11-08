;; vector%length
;; 获取向量元素总数（对标 Scala Vector.length / size）。
;;
;; 语法
;; ----
;; ($ v :length)
;;
;; 参数
;; ----
;; v : vector?
;; 源向量。
;;
;; 返回值
;; ------
;; integer?
;; 元素个数；空向量为 0。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2 3) :length) => 3)
(check ($ #() :length) => 0)

(check-report)
