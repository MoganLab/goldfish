;; vector%fold-left
;; 向量左折叠（对标 Scala Vector.foldLeft）。
;;
;; 语法
;; ----
;; ($ v :fold-left init f)
;;
;; 参数
;; ----
;; init : any
;; 初始累加器。
;;
;; f : procedure?
;; 二参函数 (acc elem)。
;;
;; v : vector?
;; 源向量。
;;
;; 返回值
;; ------
;; any
;; 最终累加器；空向量返回 init。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2 3) :fold-left 0 +) => 6)
(check ($ #(1 2 3) :fold-left 10 -) => 4)
(check ($ #() :fold-left 7 +) => 7)

(check-report)
