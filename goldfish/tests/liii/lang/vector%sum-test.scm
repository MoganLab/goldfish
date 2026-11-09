;; vector%sum
;; 向量数值求和（对标 Scala Vector.sum）。
;;
;; 语法
;; ----
;; ($ v :sum)
;;
;; 参数
;; ----
;; v : vector?
;; 源向量，元素须为数值。
;;
;; 返回值
;; ------
;; number?
;; 全部元素之和；空向量返回 0（Scala Oracle 验证）。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2 3 4) :sum) => 10)
(check ($ #() :sum) => 0)
(check ($ #(1 2 3) :map (+ _ 1) :filter (odd? _) :sum) => 3)

(check-report)
