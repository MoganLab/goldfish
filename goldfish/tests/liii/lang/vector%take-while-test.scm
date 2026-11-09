;; vector%take-while
;; 从头取向量中连续满足谓词的元素（对标 Scala Vector.takeWhile）。
;;
;; 语法
;; ----
;; ($ v :take-while p)
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
;; 最长满足前缀；首元素不满足或空向量返回 #()。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2 3) :take-while (< _ 3)) => #(1 2))
(check ($ #(5 1) :take-while (< _ 3)) => #())
(check ($ #() :take-while (> _ 0)) => #())

(check-report)
