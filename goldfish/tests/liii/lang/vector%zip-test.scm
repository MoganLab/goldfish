;; vector%zip
;; 与另一集合按短侧配对（对标 Scala Vector.zip）。
;;
;; 语法
;; ----
;; ($ v :zip other)
;;
;; 参数
;; ----
;; other : vector?/list?/string?
;; 配对集合，短侧对齐。
;;
;; v : vector?
;; 源向量。
;;
;; 返回值
;; ------
;; vector?
;; #(#(a1 b1) #(a2 b2) ...) 二元组向量，长度取两者较短者。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2) :zip #(3 4)) => '#(#(1 3) #(2 4)))
(check ($ #(1 2 3) :zip #(4)) => '#(#(1 4)))
(check ($ #() :zip #(1)) => #())
(check ($ #(1) :zip '(2)) => '#(#(1 2)))

(check-report)
