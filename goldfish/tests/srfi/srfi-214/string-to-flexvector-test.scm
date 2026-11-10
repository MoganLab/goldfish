;; string->flexvector
;; 字符串转换为可变长向量。
;;
;; 语法
;; ----
;; (string->flexvector str)
;;
;; 参数
;; ----
;; str : string
;; 待转换的字符串。
;;
;; 返回值
;; ----
;; flexvector
;; 由字符串字符组成的 flexvector。

(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


(check (flexvector->vector (string->flexvector "abc")) => #(#\a #\b #\c))


(check-report)
