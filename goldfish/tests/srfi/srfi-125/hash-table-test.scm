;; hash-table
;; 使用交替出现的键和值构造一个新的哈希表。
;;
;; 语法
;; ----
;; (hash-table key value ...)
;;
;; 参数
;; ----
;; key value ... : any
;; 交替出现的键和值。
;;
;; 返回值
;; ----
;; hash-table
;; 包含给定键值对的新哈希表。
;;
;; 注意
;; ----
;; 键和值必须成对出现；本文件只验证构造结果，不覆盖底层访问器的语义。
;;
;; 错误处理
;; ----
;; 参数个数不成对时由底层实现报错。

(import (liii check) (liii hash-table))


(check-set-mode! 'report-failed)

(let ((ht (hash-table)))
  (check (ht 'missing) => #f)
) ;let


(let ((ht (hash-table 'a 1 'b 2)))
  (check (ht 'a) => 1)
  (check (ht 'b) => 2)
) ;let


(check-report)
