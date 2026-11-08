;; vector%filter
;; 保留向量中满足谓词的元素（对标 Scala Vector.filter）。
;;
;; 语法
;; ----
;; ($ v :filter p)
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
;; vector?
;; 仅含满足 (p x) 为真元素的新向量，保持原序。
;;
;; 描述
;; ----
;; vector 路径去糖为 (vector-filter p v)。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2 3 4) :filter odd?) => #(1 3))
(check ($ #() :filter odd?) => #())
(check ($ #(1 2 3 4) :filter (<= _ 3)) => #(1 2 3))

(check-report)
