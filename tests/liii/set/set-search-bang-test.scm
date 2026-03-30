(import (liii check)
        (liii error)
        (liii set)
        (srfi srfi-128)
) ;import

(check-set-mode! 'report-failed)

;; set-search!
;; 在 set 中搜索指定元素，并通过 continuation 决定更新方式（可变操作）。
;;
;; 语法
;; ----
;; (set-search! set element failure success)
;;
;; 参数
;; ----
;; set : set
;; 目标 set。
;;
;; element : any
;; 要搜索的元素。
;;
;; failure : procedure
;; 当元素不存在时调用，接收两个 continuation：insert 与 ignore。
;;
;; success : procedure
;; 当元素存在时调用，接收 matching-element、update 与 remove。
;;
;; 返回值
;; ------
;; values
;; 返回两个值：可能更新后的 set 和 obj。
;;
;; 注意
;; ----
;; continuation 的效果：
;; (insert obj) 插入 element。
;; (ignore obj) 不修改 set。
;; (update new-element obj) 用 new-element 替换匹配元素。
;; (remove obj) 移除匹配元素。
;;
;; 示例
;; ----
;; (set-search! set element
;;   (lambda (insert ignore) (insert 'payload))
;;   (lambda (found update remove) (remove 'payload)))

(define string-ci-comparator
  (make-comparator string? string-ci=? string-ci<?
    (lambda (s) (string-hash (string-map char-downcase s)))))

;; Test set-search! insert
(define s-search-1 (set 1 2))
(call-with-values
  (lambda ()
    (set-search! s-search-1 3
      (lambda (insert ignore)
        (insert 'payload)
      ) ;lambda
      (lambda (found update remove)
        (error "unexpected success")
      ) ;lambda
    ) ;set-search!
  ) ;lambda
  (lambda (result obj)
    (check-true (eq? result s-search-1))
    (check (set-size s-search-1) => 3)
    (check-true (set-contains? s-search-1 3))
    (check-false (set-contains? s-search-1 'payload))
    (check obj => 'payload)
  ) ;lambda
) ;call-with-values

;; Test set-search! ignore
(define s-search-2 (set 1 2))
(call-with-values
  (lambda ()
    (set-search! s-search-2 3
      (lambda (insert ignore)
        (ignore 'ignored)
      ) ;lambda
      (lambda (found update remove)
        (error "unexpected success")
      ) ;lambda
    ) ;set-search!
  ) ;lambda
  (lambda (result obj)
    (check-true (eq? result s-search-2))
    (check (set-size s-search-2) => 2)
    (check-false (set-contains? s-search-2 3))
    (check obj => 'ignored)
  ) ;lambda
) ;call-with-values

;; Test set-search! update (equals but not eq?)
(define s-search-ci (list->set-with-comparator string-ci-comparator '("Apple" "Banana")))
(call-with-values
  (lambda ()
    (set-search! s-search-ci "apple"
      (lambda (insert ignore)
        (error "unexpected failure")
      ) ;lambda
      (lambda (found update remove)
        (check found => "Apple")
        (update "apple" 'updated)
      ) ;lambda
    ) ;set-search!
  ) ;lambda
  (lambda (result obj)
    (check-true (eq? result s-search-ci))
    (check (set-size s-search-ci) => 2)
    (check (set-member s-search-ci "apple" 'not-found) => "apple")
    (check obj => 'updated)
  ) ;lambda
) ;call-with-values

;; Test set-search! delete
(define s-search-3 (set 1 2 3))
(call-with-values
  (lambda ()
    (set-search! s-search-3 2
      (lambda (insert ignore)
        (error "unexpected failure")
      ) ;lambda
      (lambda (found update remove)
        (remove 'removed)
      ) ;lambda
    ) ;set-search!
  ) ;lambda
  (lambda (result obj)
    (check-true (eq? result s-search-3))
    (check (set-size s-search-3) => 2)
    (check-false (set-contains? s-search-3 2))
    (check obj => 'removed)
  ) ;lambda
) ;call-with-values

;; Test type error
(check-catch 'type-error
  (set-search! "not a set" 1
    (lambda (insert ignore) (ignore 'x))
    (lambda (found update remove) (remove 'x))
  ) ;set-search!
) ;check-catch

(check-report)
