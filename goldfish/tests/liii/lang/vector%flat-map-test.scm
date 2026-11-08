;; vector%flat-map
;; 向量映射后展平一层（对标 Scala Vector.flatMap）。
;;
;; 语法
;; ----
;; ($ v :flat-map f)
;;
;; 参数
;; ----
;; f : procedure?
;; 单参函数，返回 list/vector/string，逐元素拼接。
;;
;; v : vector?
;; 源向量。
;;
;; 返回值
;; ------
;; vector?
;; 各结果按序拼接展平一层。
;;
;;   => ($ #(1 2) :flat-map (lambda (v) (vector v v)))  ; #(1 1 2 2)

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2) :flat-map (lambda (v) (vector v v))) => #(1 1 2 2))
(check ($ #() :flat-map (lambda (v) (vector v))) => #())
(check ($ #(1 2) :flat-map (lambda (x) (list x x))) => #(1 1 2 2))

(check-report)
