(import (liii alist) (liii check))


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


(check (alist-cons 'a 1 '())
  =>
  '((a . 1))
) ;check
(check (alist-cons 'a 1 '((b . 2)))
  =>
  '((a . 1) (b . 2))
) ;check


(check (alist-cons 'key "value" '())
  =>
  '((key . "value"))
) ;check
(check (alist-cons 42 "number" '())
  =>
  '((42 . "number"))
) ;check
(check (alist-cons "string" 123 '())
  =>
  '(("string" . 123))
) ;check


(check (alist-cons 'a 1 (alist-cons 'b 2 '()))
  =>
  '((a . 1) (b . 2))
) ;check
(check (alist-cons 'c
         3
         (alist-cons 'b 2 (alist-cons 'a 1 '()))
       ) ;alist-cons
  =>
  '((c . 3) (b . 2) (a . 1))
) ;check


(check (alist-cons 'key 'value '())
  =>
  '((key . value))
) ;check


(check (alist-cons 'key 1 '((key . 99)))
  =>
  '((key . 1) (key . 99))
) ;check
(check (alist-cons 'age
         30
         '((name . "Alice") (age . 25))
       ) ;alist-cons
  =>
  '((age . 30) (name . "Alice") (age . 25))
) ;check


(check (alist-cons 'person
         '((name . "Alice") (age . 25))
         '()
       ) ;alist-cons
  =>
  '((person (name . "Alice") (age . 25)))
) ;check
(check (alist-cons 'name
         (list 'first "Alice")
         '((id . 1))
       ) ;alist-cons
  =>
  '((name first "Alice") (id . 1))
) ;check


(check (alist-cons #\a 1 '())
  =>
  '((#\a . 1))
) ;check
(check (alist-cons #t "true" '())
  =>
  '((#t . "true"))
) ;check
(check (alist-cons #f "false" '())
  =>
  '((#f . "false"))
) ;check
(check (alist-cons '(1 2 3) "list-key" '())
  =>
  '(((1 2 3) . "list-key"))
) ;check
(check (alist-cons #(1 2 3) "vector-key" '())
  =>
  '((#(1 2 3) . "vector-key"))
) ;check


(check (alist-cons 'string-value
         "Hello World"
         '()
       ) ;alist-cons
  =>
  '((string-value . "Hello World"))
) ;check
(check (alist-cons 'list-value
         '(1 2 3 4 5)
         '()
       ) ;alist-cons
  =>
  '((list-value 1 2 3 4 5))
) ;check
(check (alist-cons 'mixed-value
         '((a 1) (b 2) "string")
         '()
       ) ;alist-cons
  =>
  '((mixed-value (a 1) (b 2) "string"))
) ;check


(let ((result (alist-cons 'step3
                "final"
                (alist-cons 'step2
                  "process"
                  (alist-cons 'step1 "start" '())
                ) ;alist-cons
              ) ;alist-cons
      ) ;result
     ) ;
  (check result
    =>
    '((step3 . "final") (step2 . "process") (step1 . "start"))
  ) ;check
) ;let


(check (assq 'name
         (alist-cons 'name
           "Bob"
           '((name . "Alice") (age . 25))
         ) ;alist-cons
       ) ;assq
  =>
  '(name . "Bob")
) ;check
(check (assq 'age
         (alist-cons 'name "Bob" '((age . 30)))
       ) ;assq
  =>
  '(age . 30)
) ;check
(check (assq 'new-key
         (alist-cons 'new-key
           "value"
           '((existing . "data"))
         ) ;alist-cons
       ) ;assq
  =>
  '(new-key . "value")
) ;check


(check-report)
