(import (liii check) (liii hash-table))


(check-set-mode! 'report-failed)


;; hash-table-size
;; 返回哈希表中的条目数量。
;;
;; 语法
;; ----
;; (hash-table-size ht)
;;
;; 参数
;; ----
;; ht : hash-table
;; 目标哈希表。
;;
;; 返回值
;; ----
;; integer
;; 哈希表中当前存放的键值对数量。
;;
;; 注意
;; ----
;; 该值会随着增删操作实时变化。
;;
;; 示例
;; ----
;; (hash-table-size (make-hash-table)) => 0
;;
;; 错误处理
;; ----
;; 非哈希表输入时由底层实现报错。


(check (hash-table-size (make-hash-table))
  =>
  0
) ;check


(let ((populated-ht (make-hash-table)))
  (hash-table-set! populated-ht
    'key1
    'value1
  ) ;hash-table-set!
  (hash-table-set! populated-ht
    'key2
    'value2
  ) ;hash-table-set!
  (hash-table-set! populated-ht
    'key3
    'value3
  ) ;hash-table-set!
  (check (hash-table-size populated-ht)
    =>
    3
  ) ;check
) ;let


(check-report)
