;; vector%partition
;; 按谓词拆分向量为两组（对标 Scala Vector.partition）。
;;
;; 语法
;; ----
;; ($ v :partition p)
;;
;; 参数
;; ----
;; p : procedure?
;; 谓词；可用 `_` 占位符简写。
;;
;; v : vector?
;; 源向量。
;;
;; 返回值
;; ------
;; list?
;; 二元组 (#(满足...) #(不满足...))，各组保持原相对顺序。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2) :partition (> _ 1)) => '(#(2) #(1)))
(check ($ #() :partition (> _ 0)) => '(#() #()))
(check ($ #(2 4 6) :partition odd?) => '(#() #(2 4 6)))

(check-report)
