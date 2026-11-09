;; list%map
;; 对集合每个元素应用变换函数，返回同形新集合（对标 Scala List.map）。
;;
;; 语法
;; ----
;; ($ xs :map f)
;;
;; 参数
;; ----
;; xs : list?
;; 源链表。
;;
;; f : procedure?
;; 单参变换函数；可用 `_` 占位符简写。
;;
;; 返回值
;; ------
;; list?
;; 与 xs 等长的新链表。
;;
;; 描述
;; ----
;; 去糖为 (map f xs)。谓词位含 `_` 时自动提升为单参 lambda，
;; 如 :map (+ _ 2) 等价 (lambda (_) (+ _ 2))。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

;; 基本
(check ($ '(1 2 3) :map (lambda (x) (+ x 1))) => '(2 3 4))
(check ($ '() :map (lambda (x) (* x 2))) => '())
(check ($ '(1 2 3) :map (lambda (x) (* x 2))) => '(2 4 6))

;; `_` 占位
(check ($ '(1 2 3) :map (+ _ 2)) => '(3 4 5))
(check ($ '(1 2 3) :map (- 10 _)) => '(9 8 7))
(check ($ '(1 2 3) :map (+ (* _ 2) 1)) => '(3 5 7))
(check ($ '(1 2 3) :map _) => '(1 2 3))

;; 谓词直传（无 _ 原样）
(check ($ '(1 -2 3) :map abs) => '(1 2 3))

;; 链式组合
(check ($ '(1 2 3 4 5) :map (lambda (x) (* x x)) :filter odd?) => '(1 9 25))

(check-report)
