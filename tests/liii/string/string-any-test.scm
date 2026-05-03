(import (liii check)
        (scheme char) (liii string))

;; string-any
;; 检查字符串中的任意字符是否满足给定的条件。
;;
;; 语法
;; ----
;; (string-any char/pred? str)
;; (string-any char/pred? str start)
;; (string-any char/pred? str start end)
;;
;; 参数
;; ----
;; char/pred? : char 或 procedure?
;; - 字符(char)：检查字符串中是否存在与该字符相等的字符
;; - 谓词(procedure)：接受单个字符作为参数，返回布尔值
;;
;; str : string?
;; 要检查的字符串
;;
;; start : integer? 可选
;; 检查的起始位置(包含)，默认为0
;;
;; end : integer? 可选
;; 检查的结束位置(不包含)，默认为字符串长度
;;
;; 返回值
;; ----
;; boolean
;; - 如果字符串中至少有一个字符满足条件则返回#t，否则返回#f
;; - 对于空字符串或空范围始终返回#f
;;
;; 注意
;; ----
;; string-any是string-every的对偶函数。与检查每个字符是否满足条件的string-every不同，string-any只需要找到至少一个满足条件的字符即可返回真值。
;; 该函数也支持start和end参数来限定检查范围。
;; 空字符串或空范围会返回#f，因为没有任何字符满足条件。
;;
;; 示例
;; ----
;; (string-any char-numeric? "abc123") => #t
;; (string-any char-numeric? "hello") => #f
;; (string-any char-alphabetic? "12345a") => #t
;; (string-any char-alphabetic? "12345") => #f
;; (string-any char-upper-case? "abC12") => #t
;; (string-any char-whitespace? "hello") => #f
;; (string-any #\a "zebra") => #\a
;; (string-any #\z "apple") => #f
;;
;; 错误处理
;; ----
;; wrong-type-arg 当char/pred?不是字符或谓词时
;; out-of-range 当start/end超出字符串索引范围时
;; wrong-type-arg 当str不是字符串时
;;
;; 相关实现
;; --------
;; (liii string-cursor) 库中也提供了 string-any 函数。
;; 差异：
;; - (liii string) 版本基于字节索引遍历，start/end 参数指向字节位置，
;;   对包含多字节 Unicode 字符（如中文、emoji）的字符串，无法按字符级别精确定位。
;; - (liii string-cursor) 版本基于 cursor 遍历，支持 Unicode 字符级别的精确操作。
;; 另外，cursor 版本只接受谓词(procedure)作为第一个参数，
;; start/end 可以是整数索引或 string-cursor。
;; 参见: gf doc liii/string-cursor "string-any"

;; 字符参数
(check-true (string-any #\a "abcde"))
(check-false (string-any #\z "abcde"))

;; 谓词参数
(check-true (string-any char-numeric? "abc123"))
(check-false (string-any char-numeric? "hello"))
(check-true (string-any char-alphabetic? "12345a"))
(check-false (string-any char-alphabetic? "12345"))
(check-true (string-any (lambda (c) (char=? c #\h)) "hello"))

;; 空字符串
(check-false (string-any char-numeric? ""))

;; start/end 参数
(check-true (string-any char-alphabetic? "01c345" 2))
(check-true (string-any char-alphabetic? "01c345" 2 4))
(check-false (string-any char-alphabetic? "01c345" 3 4))
(check-false (string-any char-alphabetic? "" 0 0))

;; 错误处理
(check-catch 'wrong-type-arg (string-any 123 "hello"))
(check-catch 'wrong-type-arg (string-any char-alphabetic? 123))
(check-catch 'out-of-range (string-any char-alphabetic? "hello" -1))
(check-catch 'out-of-range (string-any char-alphabetic? "hello" 0 6))

;; Unicode 字符
(check-true (string-any char-alphabetic? "a中文b"))
(check-true (string-any char-numeric? "中文123文字"))
(check-false (string-any char-numeric? "中文测试"))
(check-true (string-any char-numeric? "123😀456"))
(check-true (string-any char-alphabetic? "a中文b" 0 6))
(check-false (string-any char-numeric? "中文测试" 0 8))

(check-report)
