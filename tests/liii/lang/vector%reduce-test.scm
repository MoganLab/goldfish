;; vector%reduce / vector%reduce-option / vector%slice
;; 向量归约与宽容切片。
;;
;; 语法
;; ----
;; ($ v :reduce f)   ($ v :reduce-option f)   ($ v :slice from until)

;; 参数
;; ----
;; f : procedure?
;; 二参归约函数 (acc elem)。
;;
;; v : vector?
;; 源向量，不可为空。
;;
;; 返回值
;; ------
;; any
;; 以首元素为种子的左结合归约结果；空向量抛 value-error
;; （安全形态用 :reduce-option，空返回 (none)）。
;;
(import (liii check) (liii lang) (liii option) (liii error))

(check-set-mode! 'report-failed)

(check ($ #(1 2 3) :reduce +) => 6)
(check-catch 'value-error ($ #() :reduce +))
(check ($ #() :reduce-option *) => (none))
(check ($ #(1 2 3 4) :slice 1 3) => #(2 3))
(check ($ #(1 2 3) :slice -1 99) => #(1 2 3))

(check-report)
