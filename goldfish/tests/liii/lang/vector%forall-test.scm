;; vector%forall
;; 判断向量是否全部元素满足谓词（对标 Scala Vector.forall）。
;;
;; 语法
;; ----
;; ($ v :forall p)
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
;; boolean
;; 所有元素满足 (p x) 为真则 #t；空向量为 #t（空真）。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2) :forall (< _ 3)) => #t)
(check ($ #(1 2) :forall (< _ 2)) => #f)
(check ($ #() :forall (> _ 0)) => #t)

(check-report)
