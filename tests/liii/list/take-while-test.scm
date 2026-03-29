(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; take-while 函数测试
;;
;; 语法
;; ----
;; (take-while pred clist)
;;
;; 参数
;; ----
;; pred : procedure?
;; 谓词函数，接受单个参数并返回布尔值。
;;
;; clist : list?
;; 要处理的列表。
;;
;; 返回值
;; ----
;; list
;; 返回从列表开头连续满足谓词条件的元素组成的新列表。
;;
;; 示例
;; ----
;; (take-while even? '()) => '()
;; (take-while (lambda (x) #t) '(1 2 3)) => '(1 2 3)
;; (take-while (lambda (x) #f) '(1 2 3)) => '()

(check (take-while even? '()) => '())

(check (take-while (lambda (x) #t) '(1 2 3))
  => '(1 2 3)
) ;check

(check
  (take-while (lambda (x) #f) '(1 2 3))
  => '()
) ;check

(check
  (take-while (lambda (x) (not (= x 1))) '(1 2 3))
  => '()
) ;check

(check
  (take-while (lambda (x) (< x 3)) '(1 2 3 0))
  => '(1 2)
) ;check

(check-report)
