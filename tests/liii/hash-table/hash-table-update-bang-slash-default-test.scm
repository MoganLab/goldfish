(import (liii check) (liii hash-table))


(check-set-mode! 'report-failed)


;; hash-table-update!/default
;; 使用默认值或当前值经由 updater 计算后，更新哈希表中的键。
;;
;; 语法
;; ----
;; (hash-table-update!/default ht key updater default)
;;
;; 参数
;; ----
;; ht : hash-table
;; 目标哈希表。
;;
;; key : any
;; 要更新的键。
;;
;; updater : procedure?
;; 接收旧值或默认值并返回新值的过程。
;;
;; default : any
;; 键不存在时提供给 updater 的默认值。
;;
;; 返回值
;; ----
;; 未指定
;; 本函数主要用于副作用。
;;
;; 注意
;; ----
;; 若键已存在，updater 会接收到当前值；否则接收到 default。
;;
;; 示例
;; ----
;; (hash-table-update!/default ht 'count add1 0) => 通过副作用写入新值
;;
;; 错误处理
;; ----
;; 非哈希表输入或 updater 不是过程时由底层实现报错。


(let ((ht (make-hash-table)))
  (hash-table-update!/default ht
    'key1
    (lambda (x) (+ x 1))
    10
  ) ;hash-table-update!/default
  (check (hash-table-ref ht 'key1) => 11)
  (hash-table-update!/default ht
    'key1
    (lambda (x) (+ x 1))
    10
  ) ;hash-table-update!/default
  (check (hash-table-ref ht 'key1) => 12)
  (hash-table-update!/default ht
    'key2
    (lambda (x) (* x 2))
    5
  ) ;hash-table-update!/default
  (check (hash-table-ref ht 'key2) => 10)
  (hash-table-update!/default ht
    'key2
    (lambda (x) (+ x 2))
    5
  ) ;hash-table-update!/default
  (check (hash-table-ref ht 'key2) => 12)
  (hash-table-update!/default ht
    'key2
    (lambda (x) #f)
    5
  ) ;hash-table-update!/default
  (check (hash-table-ref ht 'key2) => #f)
) ;let


(check-report)
