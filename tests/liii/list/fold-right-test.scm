(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; fold-right 函数测试
;;
;; 语法
;; ----
;; (fold-right kons knil clist1 clist2 ...)
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
;; 返回从右向左折叠后的最终结果。
;;
;; 说明
;; ----
;; fold-right函数从右到左遍历列表，将每个元素与累积值组合。
;;
;; 示例
;; ----
;; (fold-right + 0 '(1 2 3 4)) => 10
;; (fold-right cons () '(1 2 3 4)) => '(1 2 3 4)

(check (fold-right + 0 '(1 2 3 4)) => 10)

(check (fold-right + 0 '()) => 0)

(check
  (fold-right (lambda (x count) (if (symbol? x) (+ count 1) count))
    0
    '(a b 1 2 3 4)
  ) ;fold-right
  =>
  2
) ;check

(check (fold-right cons () '(1 2 3 4)) => '(1 2 3 4))

(check (fold-right + 0 '(1 2 3) '(4 5 6)) => 21)
(check (fold-right + 0 '(1 2 3 4) '(10 20 30)) => 66)
(check (fold-right list '() '(1 2 3) '(a b c)) => '(1 a (2 b (3 c ()))))
(check-catch 'type-error (fold-right 0 + '(1 2 3) 'a))

(check-report)
