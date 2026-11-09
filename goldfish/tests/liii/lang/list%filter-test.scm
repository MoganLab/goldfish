;; list%filter
;; 保留满足谓词的元素（对标 Scala List.filter）。
;;
;; 语法
;; ----
;; ($ xs :filter p)
;;
;; 参数
;; ----
;; xs : list?
;; 源链表。
;;
;; p : procedure?
;; 单参谓词函数；可用 `_` 占位符简写。
;;
;; 返回值
;; ------
;; list?
;; 仅含满足 (p x) 为真元素的新链表，保持原序。
;;
;; 描述
;; ----
;; 去糖为 (filter p xs)。空集或全不满足时返回 '()。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

;; 基本
(check ($ '(1 2 3 4) :filter odd?) => '(1 3))
(check ($ '() :filter odd?) => '())

;; `_` 占位
(check ($ '(1 2 3 4) :filter (<= _ 3)) => '(1 2 3))
(check ($ '(1 2 3 4 5) :filter (> _ 10)) => '())

;; 链式
(check ($ '(1 2 3 4 5) :filter odd? :map (* _ 10)) => '(10 30 50))

(check-report)
