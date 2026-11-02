;; alist-cons
;; 在关联列表前端添加一个新的键值对。
;;
;; 语法
;; ----
;; (alist-cons key datum alist)
;;
;; 参数
;; ----
;; key : any
;; 新键值对的键。
;;
;; datum : any
;; 新键值对的值。
;;
;; alist : list?
;; 已有的关联列表。
;;
;; 返回值
;; ------
;; list
;; 返回新的关联列表，新键值对位于最前面。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (alist-cons 'a 1 '()) => '((a . 1)))
(check (alist-cons 'a 1 '((b . 2))) => '((a . 1) (b . 2)))

(check (alist-cons 'key "value" '()) => '((key . "value")))
(check (alist-cons 42 "number" '()) => '((42 . "number")))
(check (alist-cons "string" 123 '()) => '(("string" . 123)))

(check (alist-cons 'a 1 (alist-cons 'b 2 '())) => '((a . 1) (b . 2)))
(check (alist-cons 'c 3 (alist-cons 'b 2 (alist-cons 'a 1 '())))
  =>
  '((c . 3) (b . 2) (a . 1)))

(check (alist-cons 'key 'value '()) => '((key . value)))

(check (alist-cons 'key 1 '((key . 99))) => '((key . 1) (key . 99)))
(check (alist-cons 'age 30 '((name . "Alice") (age . 25)))
  =>
  '((age . 30) (name . "Alice") (age . 25)))

(check (alist-cons 'person '((name . "Alice") (age . 25)) '())
  =>
  '((person (name . "Alice") (age . 25))))
(check (alist-cons 'name (list 'first "Alice") '((id . 1)))
  =>
  '((name first "Alice") (id . 1)))

(check-report)
