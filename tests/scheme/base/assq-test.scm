(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)

;;
;; assq
;; 在关联列表中查找键，使用 eq? 进行比较。
;;
;; 语法
;; ----
;; (assq key alist)
;;
;; 参数
;; ----
;; key : any
;; 要查找的键值，通常是一个符号。
;; alist : list
;; 关联列表，每个元素都是一个配对（pair），其中 car 是键，cdr 是值。
;;
;; 返回值
;; ----
;; alist-element | #f
;; 如果在关联列表中找到匹配的键，返回对应的配对；如果未找到匹配项，返回 #f。
;;
;; 描述
;; ----
;; assq 在关联列表中查找第一个键与给定 key 匹配的配对。键的比较使用 eq? 操作符，
;; 这意味着键必须是同一个对象（通常用于符号、数字或其他不可变对象）。
;;
;; assq 是 SRFI-1 规范中定义的关联列表操作函数，适用于符号键或其他.eq? 可比较的对象。
;;
;; 注意
;; ----
;; - 返回的是找到的第一个配对，不是值本身
;; - 使用 eq? 进行比较，因此最适合符号键
;; - 对于字符串键等需要使用 equal? 的情况，请使用 assoc

(let ((l '((a 1) (b 2) (c . 3))))
  (check (assq 'a l) => '(a 1))
  (check-true (eq? (assq 'a l) (list-ref l 0))
  ) ;check-true
  (check (assq 'b l) => '(b 2))
  (check (assq 'c l) => '(c . 3))
  (check (assq 'd l) => #f)
) ;let
(check (assq 'a '()) => #f)
(check (assq 'a '((a . 1))) => '(a . 1))
(check (assq 'a '((a) (b))) => '(a))
(check (assq 'b '((a 1) (b 2) (a 3)))
  =>
  '(b 2)
) ;check
(check (assq 'a '((a 1) (b 2) (a 3)))
  =>
  '(a 1)
) ;check
(check (assq 1 '((1 "one") (2 "two")))
  =>
  '(1 "one")
) ;check
(check (assq 3 '((1 "one") (2 "two")))
  =>
  #f
) ;check
(check (assq 'x '((x . 10) (y . 20)))
  =>
  '(x . 10)
) ;check
(check (assq 'vector
         '((symbol . 1) (vector . #(1 2 3)) (list a b c))
       ) ;assq
  =>
  '(vector . #(1 2 3))
) ;check
(check (assq 'list
         '((symbol . 1) (vector . #(1 2 3)) (list a b c))
       ) ;assq
  =>
  '(list a b c)
) ;check
(check (assq 'key
         '((key . value) (other . something))
       ) ;assq
  =>
  '(key . value)
) ;check
(check (assq 'missing
         '((key . value) (other . something))
       ) ;assq
  =>
  #f
) ;check
(let ((l '((a 1) (b 2) (c . 3))))
  (check (assq 'a l) => '(a 1))
  (check-true (eq? (assq 'a l) (list-ref l 0))
  ) ;check-true
  (check (assq 'b l) => '(b 2))
  (check (assq 'c l) => '(c . 3))
  (check (assq 'd l) => #f)
) ;let
(let ((l '((2 3) (5 7) (11 . 13))))
  (check (assv 5 l) => '(5 7))
  (check (assv 11 l) => '(11 . 13))
) ;let
(let ((l '(((a)) ((b)) ((c)))))
  (check (assoc '(a) l) => '((a)))
  (check (assq '(a) l) => #f)
  (check (assv '(a) l) => #f)
) ;let
(check-report)