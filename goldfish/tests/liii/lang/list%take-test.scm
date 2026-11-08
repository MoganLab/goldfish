;; list%take
;; 取前 n 个元素，越界宽容（对标 Scala List.take）。
;;
;; 语法
;; ----
;; ($ xs :take n)
;;
;; 参数
;; ----
;; xs : list?
;; 源链表。
;;
;; n : integer?
;; 要取的元素个数。
;;
;; 返回值
;; ------
;; list?
;; xs 的前 n 个元素构成的新链表。
;;
;; 描述
;; ----
;; 去糖为 (list-take xs n)，采用 (liii list) 宽容版：
;; n 超过长度返回全部，n 为负返回空表——与 Scala 逐项一致
;; （Scala Oracle 验证），不同于 srfi-1 take 的越界抛错。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

;; 基本与 Scala 宽容边界
(check ($ '(1 2 3) :take 2) => '(1 2))
(check ($ '(1 2 3) :take 0) => '())
(check ($ '(1 2 3) :take 10) => '(1 2 3))
(check ($ '(1 2 3) :take -1) => '())
(check ($ '() :take 3) => '())

;; 链式
(check ($ '(1 2 3 4 5) :take 3 :drop 1) => '(2 3))

(check-report)
