(import (liii check) (liii hash-table))


(check-set-mode! 'report-failed)


;; hash-table-copy
;; 复制一个哈希表，返回内容相同但可独立修改的新表。
;;
;; 语法
;; ----
;; (hash-table-copy ht)
;; (hash-table-copy ht mutable?)
;;
;; 参数
;; ----
;; ht : hash-table
;; 要复制的哈希表。
;;
;; mutable? : boolean? 可选
;; 保留的兼容参数；当前实现不区分返回表是否可变。
;;
;; 返回值
;; ----
;; hash-table
;; 一个新的哈希表副本。
;;
;; 注意
;; ----
;; 修改副本不会影响原表。
;;
;; 示例
;; ----
;; (hash-table-copy ht) => 副本
;;
;; 错误处理
;; ----
;; 非哈希表输入时由底层实现报错。


(let ((ht (make-hash-table)))
  (hash-table-set! ht 'k1 'v1)
  (hash-table-set! ht 'k2 'v2)
  (let ((ht-copy (hash-table-copy ht)))
    (check (hash-table-ref ht-copy 'k1)
      =>
      'v1
    ) ;check
    (check (hash-table-ref ht-copy 'k2)
      =>
      'v2
    ) ;check
    (hash-table-set! ht-copy 'k1 'modified)
    (check (hash-table-ref ht 'k1) => 'v1)
    (check (hash-table-ref ht-copy 'k1)
      =>
      'modified
    ) ;check
  ) ;let
) ;let


(let ((ht (make-hash-table)))
  (hash-table-set! ht 'a 1)
  (check (hash-table-ref (hash-table-copy ht #f)
           'a
         ) ;hash-table-ref
    =>
    1
  ) ;check
) ;let


(check-report)
