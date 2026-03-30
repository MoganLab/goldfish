(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

(define mixed-set (list->iset (iota 20 -10 3)))

;;
;; iset-search
;; 在集合中搜索元素，并使用 continuation 决定更新方式。
;;
;; 语法
;; ----
;; (iset-search iset element failure success)
;;
;; 参数
;; ----
;; iset : iset
;; 目标集合。
;;
;; element : exact-integer
;; 要搜索的元素。
;;
;; failure : procedure
;; 当元素不存在时调用，接收 insert 和 ignore 两个 continuation。
;;
;; success : procedure
;; 当元素存在时调用，接收匹配元素、update 和 remove 三个 continuation。
;;
;; 返回值
;; -----
;; 返回两个值：可能更新后的集合和 obj。
;;
;; iset-search insertion
(call-with-values
  (lambda ()
    (iset-search mixed-set
                 1
                 (lambda (insert _) (insert #t))
                 (lambda (x update _) (update 1 #t))
    ) ;iset-search
  ) ;lambda
  (lambda (set _)
    (check-true (iset=? (iset-adjoin mixed-set 1) set))
  ) ;lambda
) ;call-with-values

;; iset-search ignore
(call-with-values
  (lambda ()
    (iset-search mixed-set
                 1
                 (lambda (_ ignore) (ignore #t))
                 (lambda (x _ remove) (remove #t))
    ) ;iset-search
  ) ;lambda
  (lambda (set _)
    (check-true (iset=? mixed-set set))
  ) ;lambda
) ;call-with-values

;; iset-search remove
(call-with-values
  (lambda ()
    (iset-search mixed-set
                 2
                 (lambda (_ ignore) (ignore #t))
                 (lambda (x _ remove) (remove #t))
    ) ;iset-search
  ) ;lambda
  (lambda (set _)
    (check-true (iset=? (iset-delete mixed-set 2) set))
  ) ;lambda
) ;call-with-values

(check-report)
