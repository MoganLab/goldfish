(import (liii check) (liii comparator) (liii error) (liii hash-table))


(check-set-mode! 'report-failed)


;; make-hash-table
;; 创建一个新的哈希表，可选使用指定的比较器。
;;
;; 语法
;; ----
;; (make-hash-table)
;; (make-hash-table comparator)
;;
;; 参数
;; ----
;; comparator : comparator? 可选
;; 用于确定键相等关系和哈希函数的比较器。
;;
;; 返回值
;; ----
;; hash-table
;; 新创建的哈希表。
;;
;; 注意
;; ----
;; 传入比较器时，会使用比较器中的相等谓词和哈希函数初始化表。
;;
;; 示例
;; ----
;; (make-hash-table) => 一个空哈希表
;;
;; 错误处理
;; ----
;; type-error
;; 当参数不是比较器时抛出错误。


(let ((ht (make-hash-table)))
  (hash-table-set! ht 'a 1)
  (check (hash-table-ref ht 'a) => 1)
) ;let


(let* ((mod10 (lambda (x) (modulo x 10)))
       (digit=? (lambda (x y) (= (modulo x 10) (modulo y 10))))
       (comp (make-comparator number? digit=? #f mod10))
       (ht (make-hash-table comp))
      ) ;
  (hash-table-set! ht 1 2)
  (hash-table-set! ht 11 3)
  (check (hash-table-ref ht 1) => 3)
  (check (hash-table-ref ht 21) => 3)
) ;let*


(check-catch 'type-error (make-hash-table 1))


(let* ((count 0)
       (faulty-hash (lambda (x)
                      (set! count (+ count 1))
                      (if (= count 1) (error 'hash-fault "temporary hash error") 1)
                    ) ;lambda
       ) ;faulty-hash
       (comp (make-comparator number? = #f faulty-hash))
       (ht (make-hash-table comp))
      ) ;
  (catch 'hash-fault
    (lambda () (hash-table-set! ht 10 'val1))
    (lambda (type info) #f)
  ) ;catch
  (hash-table-set! ht 20 'val2)
  (check (hash-table-ref ht 20) => 'val2)
) ;let*




(check-report)
