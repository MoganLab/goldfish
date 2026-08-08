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


(check-catch 'type-error (fold 0 + '(1 2 3 4)))


(check (fold cons () '(1 2 3 4)) => '(4 3 2 1))


(check (fold (lambda (x count) (if (symbol? x) (+ count 1) count)) 0 '(a b
                                                                        1
                                                                        2
                                                                        3
                                                                        4))
  =>
  2
) ;check


(check (fold + 0 '(1 2 3) '(4 5 6)) => 21)
(check (fold + 0 '(1 2 3 4) '(10 20 30)) => 66)
(check (fold list '() '(1 2 3) '(a b c)) => '(3 c (2 b (1 a ()))))
(check-catch 'type-error (fold 0 + '(1 2 3) 'a))



;; 单列表点列表抛 wrong-type-arg
(check-catch 'wrong-type-arg (fold + 0 '(1 2 . 3)))

;; 单列表路径深层折叠不消耗 Scheme 栈
(let ((l (iota 100000)))
  (check (fold + 0 l) => 4999950000)
) ;let

;; GC 压力回归测试：闭包 f + 中间累加值，反复触发 GC
(let ((l (iota 10000)))
  (do ((i 0 (+ i 1)))
    ((= i 200))
    (fold (lambda (x acc) (if (even? x) (+ acc 1) acc)) 0 l)
    (fold cons '() (map (lambda (x) (list x)) '(1 2 3 4 5)))
  ) ;do
  (check (fold (lambda (x acc) (if (even? x) (+ acc 1) acc)) 0 l) => 5000)
) ;let


(check-report)
