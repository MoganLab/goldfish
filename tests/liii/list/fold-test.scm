(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; fold 函数测试
;;
;; 语法
;; ----
;; (fold kons knil clist1 clist2 ...)
;;
;; 参数
;; ----
;; kons : procedure?
;; 折叠函数，接受累积值和列表元素作为参数。
;;
;; knil : any
;; 初始累积值。
;;
;; clist1, clist2, ... : list?
;; 要折叠的列表。
;;
;; 返回值
;; ------
;; any
;; 返回折叠后的最终结果。
;;
;; 说明
;; ----
;; fold函数从左到右遍历列表，将每个元素与累积值组合。
;;
;; 示例
;; ----
;; (fold + 0 '(1 2 3 4)) => 10
;; (fold cons () '(1 2 3 4)) => '(4 3 2 1)


(check (fold + 0 '(1 2 3 4)) => 10)
(check (fold + 0 '()) => 0)


(check-catch 'type-error
  (fold 0 + '(1 2 3 4))
) ;check-catch


(check (fold cons () '(1 2 3 4))
  =>
  '(4 3 2 1)
) ;check


(check (fold (lambda (x count)
               (if (symbol? x) (+ count 1) count)
             ) ;lambda
         0
         '(a b 1 2 3 4)
       ) ;fold
  =>
  2
) ;check


(check (fold + 0 '(1 2 3) '(4 5 6))
  =>
  21
) ;check
(check (fold + 0 '(1 2 3 4) '(10 20 30))
  =>
  66
) ;check
(check (fold list '() '(1 2 3) '(a b c))
  =>
  '(3 c (2 b (1 a ())))
) ;check
(check-catch 'type-error
  (fold 0 + '(1 2 3) 'a)
) ;check-catch


(check-report)
