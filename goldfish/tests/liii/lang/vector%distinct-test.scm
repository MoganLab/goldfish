;; vector%distinct
;; 向量去重，保留首次出现顺序（对标 Scala Vector.distinct）。
;;
;; 语法
;; ----
;; ($ v :distinct)
;;
;; 参数
;; ----
;; v : vector?
;; 源向量。
;;
;; 返回值
;; ------
;; vector?
;; 去除重复元素后的新向量，保持首次出现顺序。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 1 2) :distinct) => #(1 2))
(check ($ #() :distinct) => #())
(check ($ #(1 2 3) :distinct) => #(1 2 3))

(check-report)
