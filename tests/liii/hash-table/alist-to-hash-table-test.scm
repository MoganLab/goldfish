(import (liii check)
        (liii hash-table)
) ;import

(check-set-mode! 'report-failed)

;; alist->hash-table
;; 将交替出现的键值列表转换为哈希表。
;;
;; 语法
;; ----
;; (alist->hash-table lst)
;;
;; 参数
;; ----
;; lst : list
;; 形如 (k1 v1 k2 v2 ...) 的列表。
;;
;; 返回值
;; ----
;; hash-table
;; 根据列表内容构造出的新哈希表。
;;
;; 注意
;; ----
;; 列表长度必须为偶数，否则会报错。
;;
;; 示例
;; ----
;; (alist->hash-table '(k1 v1 k2 v2)) => 一个包含 k1->v1、k2->v2 的哈希表
;;
;; 错误处理
;; ----
;; value-error
;; 当列表长度为奇数时抛出错误。
;; type-error
;; 当输入不是列表时抛出错误。

(check (hash-table-ref (alist->hash-table (list 'k1 'v1)) 'k1) => 'v1)
(check (hash-table-ref (alist->hash-table '(k1 v1 k2 v2)) 'k2) => 'v2)
(check-catch 'value-error (alist->hash-table '(k1)))

(check-report)
