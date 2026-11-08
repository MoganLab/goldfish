;; default-hash
;; 计算对象的默认哈希值。
;;
;; 语法
;; ----
;; (default-hash obj)
;;
;; 参数
;; ----
;; obj : any
;; 待计算哈希值的对象。
;; 返回值
;; ----
;; integer
;; 返回对象的哈希值。
;; 注意
;; ----
;; 相同的对象总是得到相同的哈希值。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(check-true (integer? (default-hash 1)))
(check-true (integer? (default-hash "str")))
(check-true (integer? (default-hash 'sym)))
(check-true (integer? (default-hash #t)))
(check-true (integer? (default-hash '(1 2))))

;; 相同对象哈希相同
(check (= (default-hash "abc") (default-hash (string-copy "abc"))) => #t)
(check (= (default-hash '(1 2)) (default-hash '(1 2))) => #t)

(check-report)
