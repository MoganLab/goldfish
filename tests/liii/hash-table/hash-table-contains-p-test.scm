(import (liii check)
        (liii hash-table)
) ;import

(check-set-mode! 'report-failed)

;; hash-table-contains?
;; 判断哈希表中是否存在指定键且其值不是 #f。
;;
;; 语法
;; ----
;; (hash-table-contains? ht key)
;;
;; 参数
;; ----
;; ht : hash-table
;; 目标哈希表。
;;
;; key : any
;; 要检查的键。
;;
;; 返回值
;; ----
;; boolean
;; 若键存在且对应值不是 #f，则返回 #t；否则返回 #f。
;;
;; 注意
;; ----
;; 如果键对应的值就是 #f，也会被视为“不包含”。
;;
;; 示例
;; ----
;; (hash-table-contains? ht 'brand) => #t
;;
;; 错误处理
;; ----
;; 非哈希表输入时由底层实现报错。

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'brand 'liii)
  (check-true (hash-table-contains? ht 'brand))
  (hash-table-set! ht 'brand #f)
  (check-false (hash-table-contains? ht 'brand))
) ;let

(check-false (hash-table-contains? (make-hash-table) 'missing))

(check-report)
