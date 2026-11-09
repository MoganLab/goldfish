;; boolean-hash
;; 计算布尔值的哈希值。
;;
;; 语法
;; ----
;; (boolean-hash obj)
;;
;; 参数
;; ----
;; obj : boolean?
;; 待计算哈希值的布尔值。
;; 返回值
;; ----
;; integer
;; 返回布尔值的哈希值。
;; 注意
;; ----
;; 相同布尔值的哈希值相同。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(check-true (integer? (boolean-hash #t)))
(check-true (integer? (boolean-hash #f)))

(check (= (boolean-hash #t) (boolean-hash #t)) => #t)
(check (= (boolean-hash #f) (boolean-hash #f)) => #t)

(check-report)
