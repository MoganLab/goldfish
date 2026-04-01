(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; string->flexvector
;; 将字符串转换为可变长向量。
;;
;; 语法
;; ----
;; (string->flexvector str)
;; (string->flexvector str start)
;; (string->flexvector str start end)
;;
;; 参数
;; ----
;; str : string
;; 源字符串。
;;
;; start : exact-nonnegative-integer (可选)
;; 起始索引，默认为 0。
;;
;; end : exact-nonnegative-integer (可选)
;; 结束索引，默认为字符串长度。
;;
;; 返回值
;; ----
;; flexvector
;; 字符组成的 flexvector。
;;
;; 描述
;; ----
;; 将字符串的字符转换为 flexvector。

(check (flexvector->vector (string->flexvector "abc")) => #(#\a #\b #\c))
(check (flexvector->vector (string->flexvector "")) => #())

(let ((fv (string->flexvector "hello" 1 4)))
  (check (flexvector->vector fv) => #(#\e #\l #\l))
) ;let

(check-report)
