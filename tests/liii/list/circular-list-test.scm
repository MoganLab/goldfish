(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; circular-list
;; 构造一个包含给定元素的循环列表。
;;
;; 语法
;; ----
;; (circular-list obj1 obj2 ...)
;;
;; 参数
;; ----
;; obj1, obj2, ... : any
;; 任意数量的元素，至少需要一个元素。
;;
;; 返回值
;; ----
;; pair
;; 返回一个循环列表（circular list）。循环列表的最后一个元素的cdr指向列表的第一个元素，形成一个无限循环。
;;
;; 说明
;; ----
;; circular-list是SRFI-1中定义的构造器函数，用于创建循环列表。
;; - 函数接受任意数量的元素作为参数，但至少需要一个元素
;; - 创建的列表是循环的，即最后一个元素的cdr指向第一个元素
;; - 循环列表可以通过circular-list?进行检测
;; - 循环列表在结构上是无限长度的，使用中需要特别注意避免无限循环
;;
;; 使用场景
;; --------
;; - 重复数据流的生成
;; - 环形缓冲区的实现
;; - 循环模式的模拟
;; - 算法中的周期性结构表示
;;
;; 边界情况
;; --------
;; - 当提供单个元素时，创建单个元素的循环列表
;; - 当提供多个元素时，元素按提供的顺序排列
;; - 空参数列表会抛出wrong-number-of-args错误
;;
;; 例子
;; ----
;; (circular-list 'a)        => 循环列表 (a a a ...)
;; (circular-list 'a 'b 'c)  => 循环列表 (a b c a b c ...)
;; (circular-list 1 2 3)     => 循环列表 (1 2 3 1 2 3 ...)
;;
;; 错误处理
;; --------
;; wrong-number-of-args 当没有提供参数时抛出。


(check-true (circular-list? (circular-list 1))
) ;check-true
(check-true (circular-list? (circular-list 1 2))
) ;check-true
(check-true (circular-list? (circular-list 1 2 3))
) ;check-true


(let ((cl (circular-list 1 2 3)))
  (check (cl 0) => 1)
  (check (cl 1) => 2)
  (check (cl 2) => 3)
  (check (cl 3) => 1)
  (check (cl 4) => 2)
  (check (cl 5) => 3)
  (check (cl 6) => 1)
) ;let


(check-true (circular-list? (circular-list 'a))
) ;check-true
(check-true (circular-list? (circular-list 'a 'b 'c)
            ) ;circular-list?
) ;check-true
(check-true (circular-list? (circular-list "hello" "world")
            ) ;circular-list?
) ;check-true
(check-true (circular-list? (circular-list '(1 2) '(3 4))
            ) ;circular-list?
) ;check-true


(let ((single (circular-list 'x)))
  (check (single 0) => 'x)
  (check (single 1) => 'x)
  (check (single 100) => 'x)
) ;let


(let ((nested (circular-list '(1 2) '(3) '(4 5 6))
      ) ;nested
     ) ;
  (check (nested 0) => '(1 2))
  (check (nested 1) => '(3))
  (check (nested 2) => '(4 5 6))
  (check (nested 3) => '(1 2))
) ;let


(check-catch 'wrong-number-of-args
  (circular-list)
) ;check-catch


(let ((cl (circular-list 1 2 3)))
  (check (cl 3) => 1)
  (check (cl 4) => 2)
  (check (cl 5) => 3)
  (check (cl 6) => 1)
) ;let


(check-true (circular-list? (circular-list 1 2))
) ;check-true
(check-true (circular-list? (circular-list 1))
) ;check-true


(let* ((l (list 1 2 3)) (end (last-pair l)))
  (set-cdr! end (cdr l))
  (check-true (circular-list? l))
) ;let*


(check-false (circular-list? (list 1 2))
) ;check-false


(check-report)
