(import (liii alist)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

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
;; ----
;; list
;; 返回新的关联列表，新键值对位于最前面。
;;
;; 注意
;; ----
;; 该函数不检查键唯一性，会直接把新键值对插到表头。
;;
;; 示例
;; ----
;; (alist-cons 'a 1 '()) => '((a . 1))
;; (alist-cons 'a 1 '((b . 2))) => '((a . 1) (b . 2))
;;
;; 错误处理
;; ----
;; 无

(check (alist-cons 'a 1 '()) => '((a . 1)))
(check (alist-cons 'a 1 '((b . 2))) => '((a . 1) (b . 2)))

(check (alist-cons 'key "value" '()) => '((key . "value")))
(check (alist-cons 42 "number" '()) => '((42 . "number")))
(check (alist-cons "string" 123 '()) => '(("string" . 123)))

(check (alist-cons 'a 1 (alist-cons 'b 2 '())) => '((a . 1) (b . 2)))
(check (alist-cons 'c 3 (alist-cons 'b 2 (alist-cons 'a 1 '()))) => '((c . 3) (b . 2) (a . 1)))

(check (alist-cons 'key 'value '()) => '((key . value)))

(check (alist-cons 'key 1 '((key . 99))) => '((key . 1) (key . 99)))
(check (alist-cons 'age 30 '((name . "Alice") (age . 25))) => '((age . 30) (name . "Alice") (age . 25)))

(check (alist-cons 'person '((name . "Alice") (age . 25)) '()) => '((person (name . "Alice") (age . 25))))
(check (alist-cons 'name (list 'first "Alice") '((id . 1))) => '((name first "Alice") (id . 1)))

(check (alist-cons #\a 1 '()) => '((#\a . 1)))
(check (alist-cons #t "true" '()) => '((#t . "true")))
(check (alist-cons #f "false" '()) => '((#f . "false")))
(check (alist-cons '(1 2 3) "list-key" '()) => '(((1 2 3) . "list-key")))
(check (alist-cons #(1 2 3) "vector-key" '()) => '((#(1 2 3) . "vector-key")))

(check (alist-cons 'string-value "Hello World" '()) => '((string-value . "Hello World")))
(check (alist-cons 'list-value '(1 2 3 4 5) '()) => '((list-value 1 2 3 4 5)))
(check (alist-cons 'mixed-value '((a 1) (b 2) "string") '()) => '((mixed-value (a 1) (b 2) "string")))

(let ((result (alist-cons 'step3 "final"
                          (alist-cons 'step2 "process"
                                     (alist-cons 'step1 "start" '())))
                          ) ;alist-cons
      ) ;
  (check result => '((step3 . "final") (step2 . "process") (step1 . "start")))
) ;let

(check (assq 'name (alist-cons 'name "Bob" '((name . "Alice") (age . 25)))) => '(name . "Bob"))
(check (assq 'age (alist-cons 'name "Bob" '((age . 30)))) => '(age . 30))
(check (assq 'new-key (alist-cons 'new-key "value" '((existing . "data")))) => '(new-key . "value"))

(check-report)
