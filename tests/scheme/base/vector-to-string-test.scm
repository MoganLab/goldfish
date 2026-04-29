(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; vector->string
;; 将字符向量转换为字符串。
;;
;; 语法
;; ----
;; (vector->string vector)
;;
;; 参数
;; ----
;; vector : vector?
;; 由字符组成的向量。
;;
;; 返回值
;; ------
;; string?
;; 由向量中字符按顺序组成的新字符串。
;;
;; 说明
;; ----
;; 1. 空向量返回空字符串
;; 2. 向量中每个元素必须是字符
(check (vector->string #()) => "")
(check (vector->string #(#\a)) => "a")
(check (vector->string #(#\a #\b #\c)) => "abc")
(check (vector->string #(#\1 #\2)) => "12")
(check (string-length (vector->string #(#\x #\y #\z))) => 3)
(check-catch 'wrong-type-arg (vector->string))
(check (vector->string '()) => "")
(check-catch 'wrong-type-arg (vector->string #(1 2)))

(check-report)
