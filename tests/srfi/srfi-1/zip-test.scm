;; zip
;; 将多个列表中的对应元素组合成子列表。
;;
;; 语法
;; ----
;; (zip list1 list2 ...)
;;
;; 参数
;; ----
;; list1, list2, ... : list
;; 一个或多个列表。
;;
;; 返回值
;; ------
;; list
;; 返回一个新的列表，其中的每个元素都是一个子列表，由输入列表在同一位置的元素组成。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check-catch 'type-error (zip 1 2))
(check-catch 'type-error (zip '(1 2) 3))

(check (zip '(1 2 3) '(a b c)) => '((1 a) (2 b) (3 c)))
(check (zip '(x y z) '(10 20 30)) => '((x 10) (y 20) (z 30)))

(check (zip '(1 2) '(a b c)) => '((1 a) (2 b)))
(check (zip '(1 2 3) '(a b)) => '((1 a) (2 b)))

(check (zip '() '(a b)) => '())
(check (zip '(a b) '()) => '())
(check (zip '() '()) => '())
(check (zip '() '() '()) => '())

(check (zip '(1) '(a) '(x)) => '((1 a x)))
(check (zip '(1 2 3) '(a b c) '(x y z)) => '((1 a x) (2 b y) (3 c z)))
(check (zip '(1 2) '(a b) '(x y) '(p q)) => '((1 a x p) (2 b y q)))

(check (zip '(1 2 3) '("a" "b" "c")) => '((1 "a") (2 "b") (3 "c")))
(check (zip '(#t #f) '(apple banana)) => '((#t apple) (#f banana)))
(check (zip '("hello" "world") '(42 43)) => '(("hello" 42) ("world" 43)))
(check (zip '(() (a b)) '((c d) ())) => '((() (c d)) ((a b) ())))

(check (zip '(a) '(1)) => '((a 1)))
(check (zip '(only)) => '((only)))

(let ((paired (zip '(1 2 3) '(a b c))))
  (check (map car paired) => '(1 2 3))
  (check (map cadr paired) => '(a b c)))

(check (zip '(1 2 3)) => '((1) (2) (3)))

(let ((lst1 (iota 100))
      (lst2 (iota 100 100)))
  (let ((result (zip lst1 lst2)))
    (check (length result) => 100)
    (check (car (list-ref result 0)) => 0)
    (check (cadr (list-ref result 99)) => 199)))

(check (zip '((1 2) (3 4)) '((a b) (c d))) => '(((1 2) (a b)) ((3 4) (c d))))
(check (zip '((a) b (c)) '(1 (2) 3)) => '(((a) 1) (b (2)) ((c) 3)))

(check-report)
