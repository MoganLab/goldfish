(import (liii check)
        (liii hash-table)
) ;import

(check-set-mode! 'report-failed)

;; hash-table-entries
;; 同时返回哈希表中的键列表和值列表。
;;
;; 语法
;; ----
;; (hash-table-entries ht)
;;
;; 参数
;; ----
;; ht : hash-table
;; 目标哈希表。
;;
;; 返回值
;; ----
;; 两个值
;; 第一个值是键列表，第二个值是值列表。
;;
;; 注意
;; ----
;; 结果的顺序由底层遍历顺序决定。
;;
;; 示例
;; ----
;; (hash-table-entries ht) => 两个列表值
;;
;; 错误处理
;; ----
;; 非哈希表输入时由底层实现报错。

(let ((ht (make-hash-table)))
  (check (call-with-values (lambda () (hash-table-entries ht))
                           (lambda (ks vs) (list ks vs)))
         => (list (list ) (list ))
  ) ;check

  (hash-table-set! ht 'k1 'v1)
  (check (call-with-values (lambda () (hash-table-entries ht))
                           (lambda (ks vs) (list ks vs)))
         => (list (list 'k1) (list 'v1))
  ) ;check
) ;let

(check-report)
