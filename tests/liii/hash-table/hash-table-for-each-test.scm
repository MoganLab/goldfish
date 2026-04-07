(import (liii check)
        (liii hash-table)
) ;import

(check-set-mode! 'report-failed)

;; hash-table-for-each
;; 逐个遍历哈希表中的键值对并执行副作用过程。
;;
;; 语法
;; ----
;; (hash-table-for-each proc ht)
;;
;; 参数
;; ----
;; proc : procedure?
;; 接收 key 和 value 的过程。
;;
;; ht : hash-table
;; 目标哈希表。
;;
;; 返回值
;; ----
;; 未指定
;; 本函数主要用于副作用。
;;
;; 注意
;; ----
;; 遍历顺序由底层实现决定。
;;
;; 示例
;; ----
;; (hash-table-for-each proc ht) => 通过副作用处理所有条目
;;
;; 错误处理
;; ----
;; 非哈希表输入或 proc 不是过程时由底层实现报错。

(let ((cnt 0))
  (hash-table-for-each
    (lambda (k v)
      (set! cnt (+ cnt v))
    ) ;lambda
    (hash-table 'a 1 'b 2 'c 3)
  ) ;hash-table-for-each
  (check cnt => 6)
) ;let

(check-report)
