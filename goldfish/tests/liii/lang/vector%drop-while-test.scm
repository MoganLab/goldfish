;; vector%drop-while
;; 从头跳过向量中连续满足谓词的元素（对标 Scala Vector.dropWhile）。
;;
;; 语法
;; ----
;; ($ v :drop-while p)
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
;; 去掉最长满足前缀后的剩余；全部满足返回 #()。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2 3) :drop-while (< _ 3)) => #(3))
(check ($ #(1 2 3) :drop-while (> _ 0)) => #())
(check ($ #() :drop-while (> _ 0)) => #())

(check-report)
