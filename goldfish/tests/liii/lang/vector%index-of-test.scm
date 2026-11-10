;; vector%index-of
;; 查找向量元素首次出现的下标（对标 Scala Vector.indexOf）。
;;
;; 语法
;; ----
;; ($ v :index-of e)
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
;; integer?
;; 首次出现的下标（0 起）；不存在返回 -1。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #("a" "b") :index-of "b") => 1)
(check ($ #(1 2) :index-of 9) => -1)
(check ($ #() :index-of 1) => -1)

(check-report)
