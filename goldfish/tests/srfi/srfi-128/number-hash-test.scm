;; number-hash
;; 计算数值的哈希值。
;;
;; 语法
;; ----
;; (number-hash obj)
;;
;; 参数
;; ----
;; obj : number?
;; 待计算哈希值的数值。
;; 返回值
;; ----
;; integer
;; 返回数值的哈希值。
;; 注意
;; ----
;; 相同数值的哈希值相同。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(check-true (integer? (number-hash 42)))
(check (= (number-hash 42) (number-hash 42)) => #t)
(check (= (number-hash 42) (number-hash 43)) => #f)

(check-report)
