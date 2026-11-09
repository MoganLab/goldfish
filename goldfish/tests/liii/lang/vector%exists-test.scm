;; vector%exists
;; 判断向量中是否存在满足谓词的元素（对标 Scala Vector.exists）。
;;
;; 语法
;; ----
;; ($ v :exists p)
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
;; 任一元素满足 (p x) 为真则 #t；空向量为 #f。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2 3) :exists (= _ 2)) => #t)
(check ($ #(1 2 3) :exists (= _ 9)) => #f)
(check ($ #() :exists (> _ 0)) => #f)

(check-report)
