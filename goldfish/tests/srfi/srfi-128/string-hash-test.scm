;; string-hash
;; 计算字符串的哈希值。
;;
;; 语法
;; ----
;; (string-hash obj)
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
;; 内容相同的字符串哈希值相同。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(check-true (integer? (string-hash "abc")))
(check (= (string-hash "abc") (string-hash (string-copy "abc"))) => #t)
(check (= (string-hash "abc") (string-hash "abd")) => #f)

(check-report)
