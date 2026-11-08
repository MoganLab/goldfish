;; vector%find
;; 查找向量中首个满足谓词的元素（对标 Scala Vector.find）。
;;
;; 语法
;; ----
;; ($ v :find p)
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
;; (option ...)
;; (option 首个满足元素)；不存在返回 (none)。

(import (liii check) (liii lang) (liii option))

(check-set-mode! 'report-failed)

(check ($ #(2 4) :find even?) => (option 2))
(check ($ #(1 3) :find even?) => (none))
(check ($ #() :find (> _ 0)) => (none))

(check-report)
