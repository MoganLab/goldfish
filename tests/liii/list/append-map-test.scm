(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; append-map 函数测试
;;
;; 语法
;; ----
;; (append-map f list1 list2 ...)
;;
;; 参数
;; ----
;; f : procedure?
;; 映射函数，返回列表。
;;
;; list1, list2, ... : list?
;; 要映射的列表。
;;
;; 返回值
;; ------
;; list
;; 将所有映射结果连接成一个列表。
;;
;; 说明
;; ----
;; append-map函数首先对列表进行map操作，然后将结果连接成一个列表。
;; 等价于 (apply append (map f list1 list2 ...))
;;
;; 示例
;; ----
;; (append-map (lambda (x) (list x (* x 2))) '(1 2 3)) => '(1 2 2 4 3 6)

(let* ((proc (lambda (x) (list x (* x 2))))
       (input '(1 2 3))
       (expected '(1 2 2 4 3 6)))
  (check (append-map proc input) => expected)
) ;let*

(let* ((proc (lambda (x y) (list (+ x y) (- x y))))
       (list1 '(5 8 10))
       (list2 '(3 2 7))
       (expected '(8 2 10 6 17 3)))
  (check (append-map proc list1 list2) => expected)
) ;let*

(check (append-map (lambda (x y) (list x y)) '(1) '()) => '())

(let* ((proc (lambda (x) (if (even? x) (list x) '())))
       (input '(1 2 3 4))
       (expected '(2 4)))
  (check (append-map proc input) => expected)
) ;let*

(let* ((proc (lambda (x y) (list (cons x y))))
       (list1 '(a b c))
       (list2 '(1 2))
       (expected '((a . 1) (b . 2))))
  (check (append-map proc list1 list2) => expected)
) ;let*

(let* ((proc (lambda (x) (list (list x) (list (* x 2)))))
       (input '(5))
       (expected '( (5) (10) )))
  (check (append-map proc input) => expected)
) ;let*

(check-report)
