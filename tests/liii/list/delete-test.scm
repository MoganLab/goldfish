(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; delete 函数测试
;;
;; 语法
;; ----
;; (delete obj list [=])
;;
;; 参数
;; ----
;; obj : any
;; 要删除的对象。
;;
;; list : list?
;; 要处理的列表。
;;
;; = : procedure? (可选)
;; 比较函数，默认为equal?。
;;
;; 返回值
;; ------
;; list
;; 返回删除所有等于obj的元素后的新列表。
;;
;; 示例
;; ----
;; (delete 1 (list 1 2 3 4)) => (list 2 3 4)
;; (delete 0 (list 1 2 3 4)) => (list 1 2 3 4)
;; (delete #\a (list #\a #\b #\c) char=?) => (list #\b #\c)

(check (delete 1 (list 1 2 3 4)) => (list 2 3 4))

(check (delete 0 (list 1 2 3 4)) => (list 1 2 3 4))

(check (delete #\a (list #\a #\b #\c) char=?)
       => (list #\b #\c)
) ;check

(check (delete #\a (list #\a #\b #\c) (lambda (x y) #f))
       => (list #\a #\b #\c)
) ;check

(check (delete 1 (list )) => (list ))

(check
  (catch 'wrong-type-arg
    (lambda ()
      (check (delete 1 (list 1 2 3 4) 'not-pred) => 1)
    ) ;lambda
    (lambda args #t)
  ) ;catch
  => #t
) ;check

(check-report)
