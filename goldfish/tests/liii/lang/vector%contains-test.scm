;; vector%contains
;; 判断向量是否包含指定元素（对标 Scala Vector.contains）。
;;
;; 语法
;; ----
;; ($ v :contains e)
;;
;; 参数
;; ----
;; e : any
;; 待查找元素，按 equal? 比较。
;;
;; v : vector?
;; 源向量。
;;
;; 返回值
;; ------
;; boolean
;; 存在等值元素则 #t；空向量为 #f。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2) :contains 1) => #t)
(check ($ #(1 2) :contains 3) => #f)
(check ($ #() :contains 1) => #f)

(check-report)
