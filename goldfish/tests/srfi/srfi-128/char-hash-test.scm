;; char-hash
;; 计算字符的哈希值。
;;
;; 语法
;; ----
;; (char-hash obj)
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
;; 相同字符的哈希值相同。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(check-true (integer? (char-hash #\a)))
(check-true (integer? (char-hash #\space)))

(check (= (char-hash #\a) (char-hash #\a)) => #t)
(check (= (char-hash #\a) (char-hash #\b)) => #f)

(check-report)
