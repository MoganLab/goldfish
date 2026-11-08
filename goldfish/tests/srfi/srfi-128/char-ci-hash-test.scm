;; char-ci-hash
;; 计算字符的哈希值（大小写不敏感接口）。
;;
;; 语法
;; ----
;; (char-ci-hash obj)
;;
;; 参数
;; ----
;; obj : char?
;; 待计算哈希值的字符。
;; 返回值
;; ----
;; integer
;; 返回字符的哈希值。
;; 注意
;; ----
;; 本实现的大小写不敏感哈希与 char-hash 使用同一底层哈希过程。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(check-true (integer? (char-ci-hash #\a)))
(check (= (char-ci-hash #\a) (char-ci-hash #\a)) => #t)
(check (= (char-ci-hash #\a) (char-ci-hash #\b)) => #f)

(check-report)
