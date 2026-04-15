(import (liii check) (liii hash-table))


(check-set-mode! 'report-failed)


;; hash-table->alist
;; 将哈希表转换为交替键值形式的列表。
;;
;; 语法
;; ----
;; (hash-table->alist ht)
;;
;; 参数
;; ----
;; ht : hash-table
;; 目标哈希表。
;;
;; 返回值
;; ----
;; list
;; 形如 (k1 v1 k2 v2 ...) 的列表。
;;
;; 注意
;; ----
;; 结果顺序由底层遍历顺序决定。
;;
;; 示例
;; ----
;; (hash-table->alist ht) => '(k1 v1)
;;
;; 错误处理
;; ----
;; 非哈希表输入时由底层实现报错。


(let ((ht (make-hash-table)))
  (check (hash-table->alist ht) => '())
  (hash-table-set! ht 'k1 'v1)
  (check (hash-table->alist ht)
    =>
    '(k1 v1)
  ) ;check
) ;let


(check-report)
