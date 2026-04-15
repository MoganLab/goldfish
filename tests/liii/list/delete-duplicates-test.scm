(import (liii list)
  (liii check)
  (only (srfi srfi-1) delete-duplicates)
) ;import


(check-set-mode! 'report-failed)


;; delete-duplicates 函数测试
;;
;; 语法
;; ----
;; (delete-duplicates list [=])
;;
;; 参数
;; ----
;; list : list?
;; 要处理的列表。
;;
;; = : procedure? (可选)
;; 比较函数，默认为equal?。
;;
;; 返回值
;; ------
;; list
;; 返回删除重复元素后的新列表，只保留第一次出现的元素。
;;
;; 示例
;; ----
;; (delete-duplicates (list 1 1 2 3)) => (list 1 2 3)
;; (delete-duplicates (list 1 2 3)) => (list 1 2 3)
;; (delete-duplicates (list 1 1 1)) => (list 1)
;; (delete-duplicates '(1 -2 3 2 -1) (lambda (x y) (= (abs x) (abs y)))) => (list 1 -2 3)


(check (delete-duplicates (list 1 1 2 3))
  =>
  (list 1 2 3)
) ;check
(check (delete-duplicates (list 1 2 3))
  =>
  (list 1 2 3)
) ;check
(check (delete-duplicates (list 1 1 1))
  =>
  (list 1)
) ;check


(check (delete-duplicates (list))
  =>
  (list)
) ;check


(check (delete-duplicates (list 1 1 2 3)
         (lambda (x y) #f)
       ) ;delete-duplicates
  =>
  (list 1 1 2 3)
) ;check


(check (delete-duplicates '(1 -2 3 2 -1)
         (lambda (x y) (= (abs x) (abs y)))
       ) ;delete-duplicates
  =>
  (list 1 -2 3)
) ;check


(check (catch 'wrong-type-arg
         (lambda ()
           (check (delete-duplicates (list 1 1 2 3)
                    'not-pred
                  ) ;delete-duplicates
             =>
             1
           ) ;check
         ) ;lambda
         (lambda args #t)
       ) ;catch
  =>
  #t
) ;check


(check-report)
