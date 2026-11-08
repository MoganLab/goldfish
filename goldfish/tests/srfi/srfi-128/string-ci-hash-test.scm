;; string-ci-hash
;; 计算字符串的哈希值（大小写不敏感接口）。
;;
;; 语法
;; ----
;; (string-ci-hash obj)
;;
;; 参数
;; ----
;; obj : string?
;; 待计算哈希值的字符串。
;; 返回值
;; ----
;; integer
;; 返回字符串的哈希值。
;; 注意
;; ----
;; 本实现的大小写不敏感哈希与 string-hash 使用同一底层哈希过程。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(check-true (integer? (string-ci-hash "abc")))
(check (= (string-ci-hash "abc") (string-ci-hash (string-copy "abc"))) => #t)
(check (= (string-ci-hash "abc") (string-ci-hash "abd")) => #f)

(check-report)
