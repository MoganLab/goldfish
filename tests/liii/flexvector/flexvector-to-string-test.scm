(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector->string
;; 将字符可变长向量转换为字符串。
;;
;; 语法
;; ----
;; (flexvector->string fv)
;; (flexvector->string fv start)
;; (flexvector->string fv start end)
;;
;; 参数
;; ----
;; fv : flexvector
;; 包含字符的向量。
;;
;; start : exact-nonnegative-integer (可选)
;; 起始索引，默认为 0。
;;
;; end : exact-nonnegative-integer (可选)
;; 结束索引，默认为向量长度。
;;
;; 返回值
;; ----
;; string
;; 转换后的字符串。
;;
;; 描述
;; ----
;; 将字符 flexvector 转换为字符串。

(check (flexvector->string (flexvector #\a #\b #\c)) => "abc")
(check (flexvector->string (flexvector)) => "")

(let ((fv (flexvector #\h #\e #\l #\l #\o)))
  (check (flexvector->string fv 1 4) => "ell")
) ;let

(check-report)
