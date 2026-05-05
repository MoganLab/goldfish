(import (liii check) (scheme char) (liii string))

;; string-every
;; 检查字符串中的每个字符是否都满足给定的条件。
;;
;; 语法
;; ----
;; (string-every char/pred? str)
;; (string-every char/pred? str start)
;; (string-every char/pred? str start end)
;;
;; 参数
;; ----
;; char/pred? : char 或 procedure?
;; - 字符(char)：检查字符串中的每个字符是否等于该字符
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
;; 如果字符串中的每个字符都满足条件则返回#t，否则返回#f。
;; 对于空字符串或空范围(如start=end)始终返回#t。
;;
;; 注意
;; ----
;; string-every支持多种类型的参数作为char/pred?，包括字符和谓词函数。
;; 当使用start/end参数时，检查对应子字符串的范围。
;; 空字符串或空范围会返回#t，因为没有任何字符违反条件。
;;
;; 示例
;; ----
;; (string-every #\x "xxxxxx") => #t
;; (string-every #\x "xxx0xx") => #f
;; (string-every char-numeric? "012345") => #t
;; (string-every char-numeric? "012d45") => #f
;; (string-every char-alphabetic? "abc") => #t
;; (string-every char-alphabetic? "abc123") => #f
;;
;; 错误处理
;; ----
;; wrong-type-arg 当char/pred?不是字符或谓词时
;; out-of-range 当start/end超出字符串索引范围时
;; wrong-type-arg 当str不是字符串时
;;
;; 相关实现
;; --------
;; (liii string-cursor) 库中也提供了 string-every 函数。
;; 差异：
;; - (liii string) 版本基于字节索引遍历，start/end 参数指向字节位置，
;;   对包含多字节 Unicode 字符（如中文、emoji）的字符串，无法按字符级别精确定位。
;; - (liii string-cursor) 版本基于 cursor 遍历，支持 Unicode 字符级别的精确操作。
;; 另外，cursor 版本只接受谓词(procedure)作为第一个参数，
;; start/end 可以是整数索引或 string-cursor。
;; 参见: gf doc liii/string-cursor "string-every"

;; 字符参数
(check-true (string-every #\x "xxxxxx"))
(check-false (string-every #\x "xxx0xx"))

;; 谓词参数
(check-true (string-every char-numeric? "012345"))
(check-false (string-every char-numeric? "012d45"))
(check-true (string-every char-alphabetic? "abc"))
(check-false (string-every char-alphabetic? "abc123"))
(check-true (string-every (lambda (c) (char<=? #\A c #\Z)) "ABCDEF"))

;; 空字符串
(check-true (string-every char-numeric? ""))

;; start/end 参数
(check-true (string-every char-numeric? "ab2345" 2))
(check-true (string-every char-numeric? "ab234f" 2 4))
(check-false (string-every char-numeric? "ab234f" 1 4))
(check-true (string-every char-alphabetic? "abc" 0 0))

;; 错误处理
(check-catch 'wrong-type-arg (string-every 1 "012345"))
(check-catch 'wrong-type-arg (string-every "012345" "012345"))
(check-catch 'out-of-range (string-every char-numeric? "ab234f" 2 7))
(check-catch 'out-of-range (string-every char-numeric? "ab234f" 2 1))

;; Unicode 字符
(check-true (string-every (lambda (c) #t) "一二三"))
(check-true (string-every (lambda (c) #t) "😀😃😄"))
(check-false (string-every char-alphabetic? "ab中文"))
(check-true (string-every char-alphabetic? "abc" 0 3))
(check-true (string-every char-numeric? "123😀456" 0 3))
(check-false (string-every char-numeric? "中文测试"))

(check-report)
