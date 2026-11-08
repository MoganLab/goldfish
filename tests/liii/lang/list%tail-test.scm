;; list%tail
;; 去掉首元素（对标 Scala List.tail）。
;;
;; 语法
;; ----
;; ($ xs :tail)
;;
;; 参数
;; ----
;; xs : list?
;; 源链表，不可为空。
;;
;; 返回值
;; ------
;; list?
;; 去掉首元素后的剩余；空集抛 value-error（与 Scala
;; UnsupportedOperationException 一致，报错含 :drop 1 宽容替代提示）。

(import (liii check) (liii lang) (liii error))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3) :tail) => '(2 3))
(check ($ '(1) :tail) => '())
(check-catch 'value-error ($ '() :tail))

(check-report)
