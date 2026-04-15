(import (liii check) (liii hash-table))


(check-set-mode! 'report-failed)


;; hash-table-find
;; 在哈希表中查找第一个满足条件的键值对，并返回对应的值。
;;
;; 语法
;; ----
;; (hash-table-find proc ht failure)
;;
;; 参数
;; ----
;; proc : procedure?
;; 接收 key 和 value 的判断过程。
;;
;; ht : hash-table
;; 目标哈希表。
;;
;; failure : any | procedure?
;; 没有匹配项时返回的值；如果是过程则会调用它。
;;
;; 返回值
;; ----
;; any
;; 找到时返回匹配条目的值；否则返回 failure 或 failure 的调用结果。
;;
;; 注意
;; ----
;; 只返回值，不返回键。
;;
;; 示例
;; ----
;; (hash-table-find (lambda (k v) (= v 2)) ht 'not-found) => 2
;;
;; 错误处理
;; ----
;; 非哈希表输入或 proc 不是过程时由底层实现报错。


(let ((ht (make-hash-table)))
  (hash-table-set! ht 'a 1)
  (hash-table-set! ht 'b 2)
  (hash-table-set! ht 'c 3)
  (check (hash-table-find (lambda (k v) (= v 2))
           ht
           'not-found
         ) ;hash-table-find
    =>
    2
  ) ;check
  (check (hash-table-find (lambda (k v) (= v 4))
           ht
           'not-found
         ) ;hash-table-find
    =>
    'not-found
  ) ;check
  (check (hash-table-find (lambda (k v) (eq? k 'b))
           ht
           'not-found
         ) ;hash-table-find
    =>
    2
  ) ;check
  (check (hash-table-find (lambda (k v) (eq? k 'd))
           ht
           'not-found
         ) ;hash-table-find
    =>
    'not-found
  ) ;check
  (check (hash-table-find (lambda (k v)
                            (and (symbol? k) (even? v))
                          ) ;lambda
           ht
           (lambda () 'not-found)
         ) ;hash-table-find
    =>
    2
  ) ;check
) ;let


(let ((empty-ht (make-hash-table)))
  (check (hash-table-find (lambda (k v) #t)
           empty-ht
           'empty
         ) ;hash-table-find
    =>
    'empty
  ) ;check
) ;let


(check-report)
