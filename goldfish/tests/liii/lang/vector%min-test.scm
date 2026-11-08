;; vector%min / vector%max / vector%min-option / vector%max-option
;; 向量最小/最大值（对标 Scala Vector.min/max/minOption/maxOption）。
;;
;; 语法
;; ----
;; ($ v :min)    ($ v :max)    ($ v :min-option)    ($ v :max-option)
;;
;; 参数
;; ----
;; v : vector?
;; 源向量，元素须可比较，不可为空。
;;
;; 返回值
;; ------
;; 同 list 路径：空向量 :min/:max 报错，-option 形态返回 (none)。

(import (liii check) (liii lang) (liii option) (liii error))

(check-set-mode! 'report-failed)

(check ($ #(2 1 3) :min) => 1)
(check ($ #(2 1 3) :max) => 3)
(check-catch 'value-error ($ #() :min))
(check ($ #(2 1) :min-option) => (option 1))
(check ($ #() :max-option) => (none))

(check-report)
