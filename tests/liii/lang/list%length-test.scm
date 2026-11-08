;; list%length
;; 获取元素总数（对标 Scala List.length / size）。
;;
;; 语法
;; ----
;; ($ xs :length)
;;
;; 参数
;; ----
;; xs : list?
;; 源链表。
;;
;; 返回值
;; ------
;; integer?
;; 元素个数。
;;
;; 描述
;; ----
;; 去糖为 (length xs)。空表返回 0。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3) :length) => 3)
(check ($ '() :length) => 0)
(check ($ '(1 2 3 4 5) :filter odd? :length) => 3)

(check-report)
