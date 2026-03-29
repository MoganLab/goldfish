(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; reduce-right 函数测试
;;
;; 语法
;; ----
;; (reduce-right f ridentity list)
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
;; 返回从右向左归约后的结果。
;;
;; 说明
;; ----
;; reduce-right函数与reduce类似，但从右向左进行归约。
;;
;; 示例
;; ----
;; (reduce-right + 0 '(1 2 3 4)) => 10
;; (reduce-right + 0 '()) => 0
;; (reduce-right cons () '(1 2 3 4)) => '(1 2 3 . 4)

(check (reduce-right + 0 '(1 2 3 4)) => 10)

(check (reduce-right + 0 '()) => 0)

(check (reduce-right cons () '(1 2 3 4))
       => '(1 2 3 . 4)
) ;check

(check
  (reduce-right (lambda (x count) (if (symbol? x) (+ count 1) count))
    0
    '(a b 1 2 3 4)
  ) ;reduce-right
  => 6
) ;check

(check-report)
