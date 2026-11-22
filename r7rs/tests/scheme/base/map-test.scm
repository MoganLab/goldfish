;; map
;; 测试 map 的基础高阶用法。
;;
;;
;; 语法
;; ----
;; (map proc list ...)
;;
;; 参数
;; ----
;; proc : procedure?
;; list : list?
;;
;; 返回值
;; ----
;; list
;; 返回逐项映射后的新列表。
;;
;; 注意
;; ----
;; 本文件还保留了原聚合测试中配套的局部 filter 辅助定义与断言。
;;
;; 错误处理
;; ----
;; type-error 当过程应用到不兼容参数时

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))
  ) ;cond
) ;define
(check (map (lambda (x) (* x 2)) '(1 2 3 4)) => '(2 4 6 8))
(check (map (lambda (x) (+ x 1)) '(0 1 2 3)) => '(1 2 3 4))
(check (filter (lambda (x) (> x 2)) '(1 2 3 4 5)) => '(3 4 5))
(check-catch 'type-error (map (lambda (x) (+ x 1)) '(1 2 a 4)))

;; R7RS 6.10：多列表参数之一为循环列表时，按其它有限列表的长度遍历
(let ((ls1 (list 10 100 1000)) (ls2 (list 1 2 3 4 5 6)))
  (set-cdr! (cddr ls1) ls1)
  (check (map * ls1 ls2) => '(10 200 3000 40 500 6000))
) ;let

;; 循环列表在首位时同样按有限列表长度遍历
(let ((ls1 (list 10 100 1000)) (ls2 (list 1 2 3 4 5 6)))
  (set-cdr! (cddr ls1) ls1)
  (check (map * ls2 ls1) => '(10 200 3000 40 500 6000))
) ;let

;; 循环列表参与时不产生额外迭代
(let ((ls1 (list 1 2 3)) (ls2 (list 10 20 30)))
  (set-cdr! (cddr ls1) ls1)
  (check (map * ls1 ls2) => '(10 40 90))
) ;let

;; 普通多列表仍按最短列表停止（s7 原生路径，行为不变）
(check (map * '(1 2 3 4) '(10 20 30)) => '(10 40 90))

(check-report)
