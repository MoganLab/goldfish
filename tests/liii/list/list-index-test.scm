(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; list-index 函数测试
;;
;; 语法
;; ----
;; (list-index pred clist1 clist2 ...)
;;
;; 参数
;; ----
;; pred : procedure?
;; 谓词函数，接受与列表个数相同的参数。
;;
;; clist1, clist2, ... : list?
;; 要查找的列表，必须是正规列表。
;;
;; 返回值
;; ------
;; integer? 或 #f
;; 返回第一个使 (pred e1 e2 ...) 为非 #f 的元素元组索引（从 0 开始）；
;; 没有找到则返回 #f；多列表形式在 最短 的列表结束时停止查找。
;;
;; 实现说明
;; --------
;; C 实现（src/s7_liii_list.c 的 g_list_index），多列表形式按 SRFI-1
;; 语义逐个列表同步取元素。
;;
;; 示例
;; ----
;; (list-index even? '(3 1 4 1 5 9)) => 2
;; (list-index = '(1 2 4 4 4) '(3 4 4 4 3)) => 2


;; 单列表

(check (list-index even? '(3 1 4 1 5 9)) => 2)
(check (list-index even? '()) => #f)
(check (list-index even? '(1 3 5 7 9)) => #f)
(check (list-index even? '(2 4 6)) => 0)
(check (list-index (lambda (x) (> x 0)) '(1 2 3)) => 0)
(check (list-index (lambda (x) (< x 0)) '(1 2 3)) => #f)
(check (list-index (lambda (x) (> x 2)) '(1 2 3)) => 2)
(check (integer? (list-index even? '(2))) => #t)


;; 谓词返回任意非 #f 值都算命中，并非只认 #t

(check (list-index (lambda (x) (and (even? x) x)) '(1 2 3 4)) => 1)
(check (list-index (lambda (x) (if (odd? x) 'yes #f)) '(2 4 5)) => 2)


;; 多列表：pred 每次取各列表同位置的元素，以最短列表为准

(check (list-index = '(1 2 4 4 4) '(3 4 4 4 3)) => 2)
(check (list-index = '(1 2 3) '(3 2 1)) => 1)
(check (list-index = '(1 2 3) '(1 2)) => 0)
(check (list-index = '(2 1 3) '(1 2)) => #f)
(check (list-index = '() '(1 2 3)) => #f)
(check (list-index < '(1 2 3 4) '(2 3 4 5)) => 0)
(check (list-index > '(1 2 3 4) '(2 3 4 5)) => #f)
(check (list-index (lambda (a b c) (= (+ a b) c)) '(1 2 3) '(1 2 3) '(1 4 6))
  =>
  1
) ;check
(check (list-index (lambda (a b) #f) '(a b) '(x y)) => #f)


;; 谓词执行期间分配内存（GC 压力下 anchor 保护 pred 和列表）

(check (list-index (lambda (x) (even? (car (list x)))) (iota 100)) => 0)

(define %li-pred2 (lambda (a b) (= (length (list a b)) (+ a b) 2)))
(check (list-index %li-pred2 '(1 2 3) '(1 2 3)) => 0)


;; 非法参数

;; pred 不是过程
(check-catch 'wrong-type-arg (list-index 3 '(1 2)))

;; 非列表参数
(check-catch 'wrong-type-arg (list-index even? 3))

;; 点列表：遍历到非正规尾部仍未命中即报错；命中点在尾部之前则正常返回
(check-catch 'wrong-type-arg (list-index odd? '(2 4 . 6)))
(check (list-index even? '(2 4 . 6)) => 0)

;; 多列表形式中的点列表和非列表参数（预先做正规性检查）
(check-catch 'wrong-type-arg (list-index = '(1 2 3) '(1 2 . 3)))
(check-catch 'wrong-type-arg (list-index = '(1 2 3) 3))


(check-report)
