(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; reduce 函数测试
;;
;; 语法
;; ----
;; (reduce f ridentity list)
;;
;; 参数
;; ----
;; f : procedure?
;; 归约函数，接受两个参数。
;;
;; ridentity : any
;; 当列表为空时返回的默认值。
;;
;; list : list?
;; 要归约的列表。
;;
;; 返回值
;; ------
;; any
;; 返回归约后的结果。
;;
;; 说明
;; ----
;; reduce函数与fold类似，但使用列表的第一个元素作为初始累积值（如果列表非空）。
;;
;; 示例
;; ----
;; (reduce + 0 '(1 2 3 4)) => 10
;; (reduce + 0 '()) => 0
;; (reduce cons () '(1 2 3 4)) => '(4 3 2 . 1)

(check (reduce + 0 '(1 2 3 4)) => 10)
(check (reduce + 0 '()) => 0)

(check (reduce cons () '(1 2 3 4)) => '(4 3 2 . 1))

(check-catch 'wrong-type-arg
  (reduce (lambda (x count) (if (symbol? x) (+ count 1) count))
          0
          '(a b 1 2 3 4)
  ) ;reduce
) ;check-catch

(check-report)
