(import (liii check) (liii hash-table))


(check-set-mode! 'report-failed)


;; hash-table-ref
;; 按键读取哈希表中的值。
;;
;; 语法
;; ----
;; (hash-table-ref ht key)
;;
;; 参数
;; ----
;; ht : hash-table
;; 目标哈希表。
;;
;; key : any
;; 要读取的键。
;;
;; 返回值
;; ----
;; any / #f
;; 命中时返回对应值，未命中时返回 #f。
;;
;; 注意
;; ----
;; 如果键对应的值本身是 #f，则结果与未命中相同。
;;
;; 示例
;; ----
;; (hash-table-ref ht 'key) => 'value
;;
;; 错误处理
;; ----
;; 非哈希表输入时由底层实现报错。


(let ((ht (make-hash-table)))
  (hash-table-set! ht 'key 'value)
  (check (hash-table-ref ht 'key)
    =>
    'value
  ) ;check
  (check (hash-table-ref ht 'missing)
    =>
    #f
  ) ;check
) ;let


(check-report)
