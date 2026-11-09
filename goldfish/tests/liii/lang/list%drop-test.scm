;; list%drop
;; 跳过前 n 个元素，越界宽容（对标 Scala List.drop）。
;;
;; 语法
;; ----
;; ($ xs :drop n)
;;
;; 参数
;; ----
;; xs : list?
;; 源链表。
;;
;; n : integer?
;; 要跳过的元素个数。
;;
;; 返回值
;; ------
;; list?
;; 去掉前 n 个元素后的新链表。
;;
;; 描述
;; ----
;; 去糖为 (list-drop xs n)，采用 (liii list) 宽容版：
;; n 超过长度返回空表，n 为负返回全部——与 Scala 逐项一致
;; （Scala Oracle 验证），不同于 srfi-1 drop 的越界抛错。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

;; 基本与 Scala 宽容边界
(check ($ '(1 2 3) :drop 1) => '(2 3))
(check ($ '(1 2 3) :drop 0) => '(1 2 3))
(check ($ '(1 2 3) :drop 10) => '())
(check ($ '(1 2 3) :drop -1) => '(1 2 3))
(check ($ '() :drop 3) => '())

(check-report)
