(import (liii check)
        (liii hash-table))

(check-set-mode! 'report-failed)

;; hash-table-fold
;; 以累积器的方式折叠哈希表中的所有键值对。
;;
;; 语法
;; ----
;; (hash-table-fold proc seed ht)
;;
;; 参数
;; ----
;; proc : procedure?
;; 接收 key、value 和累积值的折叠过程。
;;
;; seed : any
;; 初始累积值。
;;
;; ht : hash-table
;; 目标哈希表。
;;
;; 返回值
;; ----
;; any
;; 折叠后的最终累积结果。
;;
;; 注意
;; ----
;; fold 的累积顺序由底层遍历顺序决定。
;;
;; 示例
;; ----
;; (hash-table-fold (lambda (k v acc) (+ acc v)) 0 ht) => 6
;;
;; 错误处理
;; ----
;; 非哈希表输入或 proc 不是过程时由底层实现报错。

(let ((ht (hash-table 'a 1 'b 2 'c 3)))
  (check (hash-table-fold (lambda (k v acc) (+ acc v)) 0 ht) => 6)
) ;let

(check (hash-table-fold (lambda (k v acc) (+ acc v)) 10 (hash-table)) => 10)

(check-report)
