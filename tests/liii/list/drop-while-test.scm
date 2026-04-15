(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; drop-while 函数测试
;;
;; 语法
;; ----
;; (drop-while pred clist)
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
;; 返回从列表开头连续满足谓词条件的元素被删除后的剩余列表。
;;
;; 示例
;; ----
;; (drop-while even? '()) => '()
;; (drop-while (lambda (x) #t) '(1 2 3)) => '()
;; (drop-while (lambda (x) #f) '(1 2 3)) => '(1 2 3)


(check (drop-while even? '()) => '())


(check (drop-while (lambda (x) #t) '(1 2 3))
  =>
  '()
) ;check


(check (drop-while (lambda (x) #f) '(1 2 3))
  =>
  '(1 2 3)
) ;check


(check (drop-while (lambda (x) (not (= x 1)))
         '(1 2 3)
       ) ;drop-while
  =>
  '(1 2 3)
) ;check


(check-report)
