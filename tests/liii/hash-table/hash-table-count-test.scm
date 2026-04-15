(import (liii check) (liii hash-table))


(check-set-mode! 'report-failed)


;; hash-table-count
;; 统计哈希表中满足谓词的条目数。
;;
;; 语法
;; ----
;; (hash-table-count pred? ht)
;;
;; 参数
;; ----
;; pred? : procedure?
;; 接收 key 和 value 的谓词。
;;
;; ht : hash-table
;; 目标哈希表。
;;
;; 返回值
;; ----
;; integer
;; 满足谓词的条目数量。
;;
;; 注意
;; ----
;; 谓词会同时收到键和值。
;;
;; 示例
;; ----
;; (hash-table-count (lambda (k v) #t) ht) => 3
;;
;; 错误处理
;; ----
;; 非哈希表输入或 pred? 不是过程时由底层实现报错。


(check (hash-table-count (lambda (k v) #f)
         (hash-table)
       ) ;hash-table-count
  =>
  0
) ;check
(check (hash-table-count (lambda (k v) #t)
         (hash-table 'a 1 'b 2 'c 3)
       ) ;hash-table-count
  =>
  3
) ;check
(check (hash-table-count (lambda (k v) #f)
         (hash-table 'a 1 'b 2 'c 3)
       ) ;hash-table-count
  =>
  0
) ;check


(check (hash-table-count (lambda (k v) (eq? k 'b))
         (hash-table 'a 1 'b 2 'c 3)
       ) ;hash-table-count
  =>
  1
) ;check


(check (hash-table-count (lambda (k v) (> v 1))
         (hash-table 'a 1 'b 2 'c 3)
       ) ;hash-table-count
  =>
  2
) ;check


(check (hash-table-count (lambda (k v) (string? k))
         (hash-table "apple" 1 "banana" 2)
       ) ;hash-table-count
  =>
  2
) ;check


(check (hash-table-count (lambda (k v)
                           (and (symbol? k) (even? v))
                         ) ;lambda
         (hash-table 'apple
           2
           'banana
           3
           'cherry
           4
         ) ;hash-table
       ) ;hash-table-count
  =>
  2
) ;check


(check (hash-table-count (lambda (k v) (eq? k v))
         (hash-table 'a 'a 'b 'b 'c 'd)
       ) ;hash-table-count
  =>
  2
) ;check


(check (hash-table-count (lambda (k v) (number? k))
         (hash-table 1 100 2 200 3 300)
       ) ;hash-table-count
  =>
  3
) ;check


(check (hash-table-count (lambda (k v) (list? v))
         (hash-table 'a '(1 2) 'b '(3 4) 'c 3)
       ) ;hash-table-count
  =>
  2
) ;check


(check (hash-table-count (lambda (k v)
                           (= (char->integer (string-ref (symbol->string k) 0)
                              ) ;char->integer
                             v
                           ) ;=
                         ) ;lambda
         (hash-table 'a 97 'b 98 'c 99)
       ) ;hash-table-count
  =>
  3
) ;check


(check-report)
