;; vector%map
;; 对向量每个元素应用变换函数，返回同形新向量（对标 Scala Vector.map）。
;;
;; 语法
;; ----
;; ($ v :map f)
;;
;; 参数
;; ----
;; v : vector?
;; 源向量。
;;
;; f : procedure?
;; 单参变换函数；可用 `_` 占位符简写。
;;
;; 返回值
;; ------
;; vector?
;; 与 v 等长的新向量。
;;
;; 描述
;; ----
;; 双路径 helper 按运行时类型分流，vector 路径去糖为 (vector-map f v)。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2 3) :map (lambda (x) (+ x 1))) => #(2 3 4))
(check ($ #() :map (lambda (x) (* x 2))) => #())
(check ($ #(1 2 3) :map (* _ 2)) => #(2 4 6))
(check ($ #(1 2 3) :map (- 10 _)) => #(9 8 7))
(check ($ #(1 2 3) :map _) => #(1 2 3))

(check-report)
