;; vector%par-map / vector%par-filter / vector%par-for-each
;; 向量多核并行算子（对标 Scala Parallel Collections）。
;;
;; 语法
;; ----
;; ($ v :par-map f)   ($ v :par-filter p)   ($ v :par-for-each f)

;; 参数
;; ----
;; f : procedure?
;; 单参变换函数；可用 `_` 占位符简写。
;;
;; v : vector?
;; 源向量。
;;
;; 返回值
;; ------
;; vector? / unspecified
;; :par-map / :par-filter 返回新向量；
;; :par-for-each 施加副作用后返回原向量（可续链）。
;;
(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2 3) :par-map (+ _ 10)) => #(11 12 13))
(check ($ #(1 2 3 4 5 6) :par-filter (lambda (x) (odd? x))) => #(1 3 5))
(check ($ #() :par-map (lambda (x) x)) => #())

(check-report)
