(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; count 函数测试
;;
;; 语法
;; ----
;; (count pred clist1 clist2 ...)
;;
;; 参数
;; ----
;; pred : procedure?
;; 谓词函数，接受与列表个数相同的参数。
;;
;; clist1, clist2, ... : list?
;; 要计数的列表，必须是正规列表。
;;
;; 返回值
;; ------
;; integer?
;; 返回使 (pred e1 e2 ...) 为真的元素元组个数；
;; 多列表形式在 最短 的列表结束时停止计数。
;;
;; 实现说明
;; --------
;; C 实现（src/s7_liii_list.c 的 g_count），多列表形式按 SRFI-1
;; 语义逐个列表同步取元素。
;;
;; 示例
;; ----
;; (count even? '(3 1 4 1 5 9 2 5 6)) => 3
;; (count = '(1 2 4 4 4) '(3 4 4 4 3)) => 2


;; 单列表

(check (count even? '(3 1 4 1 5 9 2 5 6)) => 3)
(check (count even? '()) => 0)
(check (count even? '(1 3 5)) => 0)
(check (count even? '(2 4 6)) => 3)
(check (count (lambda (x) (> x 0)) '(1 2 3)) => 3)
(check (count (lambda (x) (< x 0)) '(1 2 3)) => 0)
(check (integer? (count even? '(1 2))) => #t)


;; 谓词返回任意非 #f 值都计数，并非只计 #t

(check (count (lambda (x) (and (even? x) x)) '(1 2 3 4)) => 2)
(check (count (lambda (x) (if (odd? x) 'yes #f)) '(1 2 3)) => 2)


;; 多列表：pred 每次取各列表同位置的元素，以最短列表为准

(check (count = '(1 2 3 4 5) '(3 4 5 6 7)) => 0)
(check (count = '(1 2 4 4 4) '(3 4 4 4 3)) => 2)
(check (count = '(1 2 3) '(3 2 1)) => 1)
(check (count = '(1 2 3) '(1 2)) => 2)
(check (count = '(1 2) '(1 2 3)) => 2)
(check (count = '() '(1 2 3)) => 0)
(check (count < '(1 2 3 4) '(2 3 4 5)) => 4)
(check (count (lambda (a b c) (= (+ a b) c)) '(1 2 3) '(1 2 3) '(2 4 6)) => 3)
(check (count (lambda (a b) #t) '(a b) '(x y)) => 2)


;; 谓词执行期间分配内存（GC 压力下 anchor 保护 pred 和列表）

(check (count (lambda (x) (even? (car (list x)))) (iota 100)) => 50)
(check (count (lambda (a b) (= (length (list a b)) (+ a b) 2)) '(1 2 3) '(1 2 3))
  =>
  1
) ;check


;; 非法参数

;; pred 不是过程
(check-catch 'wrong-type-arg (count 3 '(1 2)))

;; 非列表参数
(check-catch 'wrong-type-arg (count even? 3))

;; 点列表：count 总是遍历完整列表，到达非正规尾部即报错
(check-catch 'wrong-type-arg (count even? '(1 2 . 3)))
(check-catch 'wrong-type-arg (count even? '(2 4 . 6)))

;; 多列表形式中的点列表和非列表参数
(check-catch 'wrong-type-arg (count = '(1 2 3) '(1 2 . 3)))
(check-catch 'wrong-type-arg (count = '(1 2 3) 3))


(check-report)
