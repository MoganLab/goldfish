;; symbol-hash
;; 计算符号的哈希值。
;;
;; 语法
;; ----
;; (symbol-hash obj)
;;
;; 参数
;; ----
;; obj : symbol?
;; 待计算哈希值的符号。
;; 返回值
;; ----
;; integer
;; 返回符号的哈希值。
;; 注意
;; ----
;; 相同符号的哈希值相同。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(check-true (integer? (symbol-hash 'abc)))
(check (= (symbol-hash 'abc) (symbol-hash 'abc)) => #t)
(check (= (symbol-hash 'abc) (symbol-hash 'abd)) => #f)

(check-report)
