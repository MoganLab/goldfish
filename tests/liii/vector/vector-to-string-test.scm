(import (liii check)
        (liii vector)
) ;import

(check-set-mode! 'report-failed)

;; vector->string
;; 将字符向量转换为字符串。
;;
;; 语法
;; ----
;; (vector->string vec)
;; (vector->string vec start)
;; (vector->string vec start end)
;;
;; 参数
;; ----
;; vec : vector?
;; 要转换的字符向量。
;;
;; start : integer? 可选
;; 起始位置（包含），默认为0。
;;
;; end : integer? 可选
;; 结束位置（不包含），默认为向量长度。
;;
;; 返回值
;; ----
;; string
;; 由指定区间字符构成的新字符串。
;;
;; 注意
;; ----
;; 被转换的元素必须都是字符。
;;
;; 示例
;; ----
;; (vector->string (vector #\\a #\\b #\\c)) => "abc"
;; (vector->string (vector #\\0 #\\1 #\\2 #\\3) 1 3) => "12"
;;
;; 错误处理
;; ----
;; out-of-range 当start/end超出向量边界或start大于end时
;; wrong-type-arg 当vec不是向量、start/end不是整数，或区间内含有非字符元素时

(check (vector->string (vector #\0 #\1 #\2 #\3)) => "0123")
(check (vector->string (vector #\a #\b #\c)) => "abc")
(check (vector->string (vector #\0 #\1 #\2 #\3) 0 4) => "0123")
(check (vector->string (vector #\0 #\1 #\2 #\3) 1) => "123")
(check (vector->string (vector #\0 #\1 #\2 #\3) 1 4) => "123")
(check (vector->string (vector #\0 #\1 #\2 #\3) 1 3) => "12")
(check (vector->string (vector #\0 #\1 #\2 #\3) 1 2) => "1")
(check-catch 'out-of-range (vector->string (vector #\0 #\1 #\2 #\3) 2 10))
(check (vector->string (vector 0 1 #\2 3 4) 2 3) => "2")
(check-catch 'wrong-type-arg (vector->string (vector 0 1 #\2 3 4) 1 3))

(check-report)
