;; list%head / list%last / list%head-option / list%last-option
;; 首末元素（对标 Scala head/last/headOption/lastOption）。
;;
;; 语法
;; ----
;; ($ xs :head)   ($ xs :last)   ($ xs :head-option)   ($ xs :last-option)
;;
;; 参数
;; ----
;; xs : list?
;; 源链表，不可为空。
;;
;; 返回值
;; ------
;; :head/:last —— 空集抛 value-error（提示 -option 形态）；
;; :head-option/:last-option —— (option v)，空集 (none)。
;; option 主语：:head 即取值（空抛错），与 get 同义。

(import (liii check) (liii lang) (liii option) (liii error))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3) :head) => 1)
(check ($ '(1 2 3) :last) => 3)
(check ($ '(5) :head) => 5)
(check ($ '(5) :last) => 5)
(check ($ '(1 2 3) :filter (> _ 2) :head) => 3)
(check-catch 'value-error ($ '() :head))
(check-catch 'value-error ($ '() :last))
(check ($ '(1 2 3) :head-option) => (option 1))
(check ($ '(1 2 3) :last-option) => (option 3))
(check ($ '() :head-option) => (none))
(check ($ '() :last-option) => (none))
(check ($ '(1 2 3) :head-option :get-or-else -1) => 1)

(check-report)
