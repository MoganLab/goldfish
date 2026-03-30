(import (liii check)
        (liii hash-table))

(check-set-mode! 'report-failed)

;; hash-table-map->list
;; 将哈希表中的每个键值对映射为列表元素。
;;
;; 语法
;; ----
;; (hash-table-map->list proc ht)
;;
;; 参数
;; ----
;; proc : procedure?
;; 接收 key 和 value 的映射过程。
;;
;; ht : hash-table
;; 目标哈希表。
;;
;; 返回值
;; ----
;; list
;; 由映射结果组成的列表。
;;
;; 注意
;; ----
;; 结果顺序由底层遍历顺序决定。
;;
;; 示例
;; ----
;; (hash-table-map->list (lambda (k v) k) ht) => '(a b c)
;;
;; 错误处理
;; ----
;; 非哈希表输入或 proc 不是过程时由底层实现报错。

(let* ((ht (hash-table 'a 1 'b 2 'c 3))
       (ks (hash-table-map->list (lambda (k v) k) ht))
       (vs (hash-table-map->list (lambda (k v) v) ht)))
  (check-true (not (null? (member 'a ks))))
  (check-true (not (null? (member 'b ks))))
  (check-true (not (null? (member 'c ks))))
  (check-true (not (null? (member 1 vs))))
  (check-true (not (null? (member 2 vs))))
  (check-true (not (null? (member 3 vs))))
) ;let*

(check-report)
