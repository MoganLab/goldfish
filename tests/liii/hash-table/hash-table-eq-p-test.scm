(import (liii check) (liii hash-table))


(check-set-mode! 'report-failed)


;; hash-table=?
;; 判断两个哈希表的结构和值是否相等。
;;
;; 语法
;; ----
;; (hash-table=? ht1 ht2)
;;
;; 参数
;; ----
;; ht1 ht2 : hash-table
;; 待比较的两个哈希表。
;;
;; 返回值
;; ----
;; boolean
;; 两个表内容相等时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 这里比较的是哈希表的内容，而不是对象是否同一。
;;
;; 示例
;; ----
;; (hash-table=? ht1 ht2) => #t
;;
;; 错误处理
;; ----
;; 非哈希表输入时由底层实现报错。


(let ((empty-h1 (make-hash-table))
      (empty-h2 (make-hash-table))
     ) ;
  (check (hash-table=? empty-h1 empty-h2)
    =>
    #t
  ) ;check
) ;let


(let ((t1 (make-hash-table))
      (t2 (make-hash-table))
     ) ;
  (hash-table-set! t1 'a 1)
  (hash-table-set! t2 'a 1)
  (check (hash-table=? t1 t2) => #t)
  (hash-table-set! t1 'b 2)
  (check (hash-table=? t1 t2) => #f)
) ;let


(check-report)
