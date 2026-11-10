;; list%partition
;; 按谓词拆分为两组（对标 Scala List.partition）。
;;
;; 语法
;; ----
;; ($ xs :partition p)
;;
;; 参数
;; ----
;; p : procedure?
;; 单参谓词；可用 `_` 占位符简写。
;;
;; 返回值
;; ------
;; list?
;; 二元组 ((满足元素...) (不满足元素...))，各组保持原相对顺序。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3 4) :partition even?) => '((2 4) (1 3)))
(check ($ '() :partition (> _ 0)) => '(() ()))
(check ($ '(1 3 5) :partition even?) => '(() (1 3 5)))
(check ($ '(2 4) :partition even?) => '((2 4) ()))

(check-report)
