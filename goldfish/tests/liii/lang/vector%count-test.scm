;; vector%count
;; 统计向量中满足谓词的元素个数（对标 Scala Vector.count）。
;;
;; 语法
;; ----
;; ($ v :count p)
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
;; integer?
;; 满足 (p x) 为真的元素个数。
;;
;; 描述
;; ----
;; vector 路径去糖为 (vector-count p v)（srfi-133）。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2 3 4) :count odd?) => 2)
(check ($ #() :count odd?) => 0)
(check ($ #(1 2 3 4 5) :count (<= _ 3)) => 3)

(check-report)
