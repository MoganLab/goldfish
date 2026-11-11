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
;; 说明
;; ----
;; 与 S7 原生函数 `s7-make-hash-table` 的区别：
;; - `make-hash-table` 创建的是 Goldfish 自己的哈希表，支持比较器（comparator），
;;   可以配合 hash-table-ref、hash-table-update! 等完整 API 使用。
;; - `s7-make-hash-table` 创建的是 S7 原生哈希表，初始化更快，但只支持 S7 的
;;   基础接口（等价比较 + 固定容量），需要用 `s7-hash-table-set!` 写入。
;; - 两种哈希表互不通用，不能把 `make-hash-table` 得到的表交给 `s7-hash-table-set!`。
;;
;; 选择建议：常规代码用 `make-hash-table`（功能完整、支持自定义比较器）；
;; 需要与 S7 内建函数共享同一张原生哈希表或追求原生性能时用 `s7-make-hash-table`。
;;
;; 参见: gf doc liii/s7 "s7-make-hash-table"
;;
;; 错误处理
;; ----
;; type-error
;; 当参数不是比较器时抛出错误。

(import (liii check) (liii comparator) (liii error) (liii hash-table))


(check-set-mode! 'report-failed)

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
