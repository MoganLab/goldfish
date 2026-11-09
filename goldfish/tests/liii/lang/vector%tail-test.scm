;; vector%tail
;; 去掉向量首元素（对标 Scala Vector.tail）。
;;
;; 语法
;; ----
;; ($ v :tail)
;;
;; 参数
;; ----
;; v : vector?
;; 源向量，不可为空。
;;
;; 返回值
;; ------
;; vector?
;; 去掉首元素的新向量；空向量抛 value-error。

(import (liii check) (liii lang) (liii error))

(check-set-mode! 'report-failed)

(check ($ #(1 2) :tail) => #(2))
(check ($ #(1) :tail) => #())
(check-catch 'value-error ($ #() :tail))

(check-report)
