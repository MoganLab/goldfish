;; list%reduce / list%reduce-option / list%slice
;; 归约（空集报错/none）与宽容切片。
;;
;; 语法
;; ----
;; ($ xs :reduce f)   ($ xs :reduce-option f)   ($ xs :slice from until)
;;
;; 参数
;; ----
;; f : procedure?
;; 二参归约函数 (acc elem)。
;;
;; xs : list?
;; 源链表，不可为空。
;;
;; 返回值
;; ------
;; :reduce —— 空集抛 value-error（对标 Scala NoSuchElementException）；
;; :reduce-option —— 空集 (none)；
;; :slice —— [from, until) 宽容 clamp，越界/负数不抛错。

(import (liii check) (liii lang) (liii option) (liii error))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3) :reduce +) => 6)
(check ($ '(1 2 3) :reduce -) => -4)
(check ($ '(5) :reduce *) => 5)
(check-catch 'value-error ($ '() :reduce +))
(check ($ '() :reduce-option +) => (none))
(check ($ '(1 2 3) :reduce-option +) => (option 6))
(check ($ '(1 2 3) :slice 1 3) => '(2 3))
(check ($ '(1 2 3) :slice -5 2) => '(1 2))
(check ($ '(1 2 3) :slice 2 99) => '(3))
(check ($ '(1 2 3) :slice 2 1) => '())

(check-report)
