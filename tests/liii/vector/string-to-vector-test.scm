(import (liii check)
        (liii vector)
) ;import

(check-set-mode! 'report-failed)

;; string->vector
;; 将字符串转换为字符向量。
;;
;; 语法
;; ----
;; (string->vector str)
;; (string->vector str start)
;; (string->vector str start end)
;;
;; 参数
;; ----
;; str : string?
;; 要转换的字符串。
;;
;; start : integer? 可选
;; 起始位置（包含），默认为0。
;;
;; end : integer? 可选
;; 结束位置（不包含），默认为字符串长度。
;;
;; 返回值
;; ----
;; vector
;; 由指定区间字符构成的新向量。
;;
;; 注意
;; ----
;; 返回的是字符向量，元素类型为char。
;;
;; 示例
;; ----
;; (string->vector "abc") => #( #\\a #\\b #\\c )
;; (string->vector "0123" 1 3) => #( #\\1 #\\2 )
;;
;; 错误处理
;; ----
;; out-of-range 当start/end超出字符串边界或start大于end时
;; wrong-type-arg 当str不是字符串，或start/end不是整数时

(check (string->vector "0123") => (vector #\0 #\1 #\2 #\3))
(check (string->vector "abc") => (vector #\a #\b #\c))
(check (string->vector "0123" 0 4) => (vector #\0 #\1 #\2 #\3))
(check (string->vector "0123" 1) => (vector #\1 #\2 #\3))
(check (string->vector "0123" 1 4) => (vector #\1 #\2 #\3))
(check (string->vector "0123" 1 3) => (vector #\1 #\2))
(check (string->vector "0123" 1 2) => (vector #\1))
(check-catch 'out-of-range (string->vector "0123" 2 10))

(check-report)
