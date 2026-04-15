(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; map
;; 测试 map 的基础高阶用法。
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
;; 示例
;; ----
;; (map (lambda (x) (* x 2)) '(1 2 3)) => '(2 4 6)
;;
;; 错误处理
;; ----
;; wrong-type-arg 当过程应用到不兼容参数时
(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst) (filter pred (cdr lst)))
        ) ;
        (else (filter pred (cdr lst)))
  ) ;cond
) ;define
(check (map (lambda (x) (* x 2)) '(1 2 3 4))
  =>
  '(2 4 6 8)
) ;check
(check (map (lambda (x) (+ x 1)) '(0 1 2 3))
  =>
  '(1 2 3 4)
) ;check
(check (filter (lambda (x) (> x 2))
         '(1 2 3 4 5)
       ) ;filter
  =>
  '(3 4 5)
) ;check
(check-catch 'wrong-type-arg
  (map (lambda (x) (+ x 1)) '(1 2 a 4))
) ;check-catch
(check-report)