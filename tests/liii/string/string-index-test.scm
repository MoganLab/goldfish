(import (liii check)
        (liii string)
) ;import

;; string-index
;; 在字符串中查找指定字符或满足条件的第一个字符的位置。
;;
;; 语法
;; ----
;; (string-index str char/pred?)
;; (string-index str char/pred? start)
;; (string-index str char/pred? start end)
;;
;; 参数
;; ----
;; str : string?
;; 要搜索的源字符串。
;;
;; char/pred? : char? 或 procedure?
;; - 字符(char)：要查找的目标字符
;; - 谓词(procedure)：接受单个字符作为参数的函数，返回布尔值指示是否匹配
;;
;; start : integer? 可选
;; 搜索的起始位置(包含)，默认为0。
;;
;; end : integer? 可选
;; 搜索的结束位置(不包含)，默认为字符串长度。
;;
;; 返回值
;; ----
;; integer 或 #f
;; - 如果找到匹配的字符，返回其索引位置(从0开始计数)
;; - 如果未找到匹配的字符，返回#f
;;
;; 注意
;; ----
;; string-index从字符串的左侧(开头)开始搜索，返回第一个匹配字符的索引位置。
;; 搜索范围由start和end参数限定。空字符串或未找到匹配项时返回#f。
;;
;; 该函数支持使用字符和谓词两种方式进行查找:
;; - 字符匹配：查找与指定字符相等的字符
;; - 谓词匹配：查找使谓词返回#t的第一个字符
;;
;; 示例
;; ----
;; (string-index "hello" #\e) => 1  (字符'e'在索引1处)
;; (string-index "hello" #\z) => #f (没有找到字符'z')
;; (string-index "abc123" char-numeric?) => 3 (第一个数字'1'在索引3处)
;; (string-index "hello" char-alphabetic?) => 0 (第一个字母'h'在索引0处)
;; (string-index "hello" #\l 2) => 3 (从索引2开始找前字符'l')
;; (string-index "hello" #\l 0 2) => #f (在0到2范围内没有找到'l')
;; (string-index "" #\x) => #f (空字符串返回#f)
;;
;; 错误处理
;; ----
;; wrong-type-arg 当str不是字符串类型时
;; wrong-type-arg 当char/pred?不是字符或谓词时
;; out-of-range 当start/end超出字符串索引范围时

; Basic functionality tests
(check (string-index "hello" #\e) => 1)
(check (string-index "hello" #\z) => #f)
(check (string-index "hello" #\l) => 2)
(check (string-index "hello" #\l 3) => 3)
(check (string-index "abc123" char-numeric?) => 3)
(check (string-index "abc123" char-alphabetic?) => 0)
(check (string-index "" #\x) => #f)

; Character parameter tests
(check (string-index "0123456789" #\2) => 2)
(check (string-index "0123456789" #\2 2) => 2)
(check (string-index "0123456789" #\2 3) => #f)
(check (string-index "01x3456789" char-alphabetic?) => 2)

; Extended comprehensive tests
(check (string-index "hello" #\h) => 0)
(check (string-index "hello" #\o) => 4)
(check (string-index "hello hello" #\space) => 5)
(check (string-index "hello" #\H) => #f) ; case-sensitive
(check (string-index "" #\a) => #f)
(check (string-index "a" #\a) => 0)
(check (string-index "aaaa" #\a) => 0)
(check (string-index "0123456789" #\0) => 0)
(check (string-index "0123456789" #\9) => 9)

; Predicate parameter tests
(check (string-index "0123456789" char-numeric?) => 0)
(check (string-index "abc123" char-numeric?) => 3)
(check (string-index "123abc" char-alphabetic?) => 3)
(check (string-index "Hello123" char-upper-case?) => 0)
(check (string-index "hello123" char-upper-case?) => #f)
(check (string-index "123!@#" char-alphabetic?) => #f)
(check (string-index "hello\n\t " char-whitespace?) => 5)
(check (string-index "hello" (lambda (c) (char=? c #\l))) => 2)

; Single character edge cases
(check (string-index "a" #\a) => 0)
(check (string-index "a" #\b) => #f)
(check (string-index " " #\space) => 0)
(check (string-index "\t" char-whitespace?) => 0)

; Start and end parameter tests
(check (string-index "hello" #\l 0) => 2)
(check (string-index "hello" #\l 1) => 2)
(check (string-index "hello" #\l 2) => 2)
(check (string-index "hello" #\l 3) => 3)
(check (string-index "hello" #\l 4) => #f)
(check (string-index "hello" #\l 5) => #f)
(check (string-index "hello" #\l 0 3) => 2)
(check (string-index "hello" #\l 0 2) => #f)
(check (string-index "hello" #\l 1 4) => 2)
(check (string-index "hello" #\l 2 4) => 2)
(check (string-index "hello" #\l 3 4) => 3)
(check (string-index "hello" #\l 3 3) => #f)

; Special characters and edge cases
(check (string-index "_test" #\_) => 0)
(check (string-index "a@b" #\@) => 1)
(check (string-index "hello,world" #\,) => 5)
(check (string-index "a-b-c" #\-) => 1)

; Complex predicates
(check (string-index "123abc!@#" (lambda (c) (or (char-alphabetic? c) (char-numeric? c)))) => 0)
(check (string-index "!@#abc123" (lambda (c) (or (char-alphabetic? c) (char-numeric? c)))) => 3)
(check (string-index "abc123" char-upper-case?) => #f)
(check (string-index "ABC123" char-upper-case?) => 0)
(check (string-index "abcABC" char-upper-case?) => 3)

; Empty string and boundary conditions
(check (string-index "" char-alphabetic?) => #f)
(check (string-index "" char-numeric?) => #f)
(check (string-index "abc" char-whitespace?) => #f)
(check (string-index "12345" char-alphabetic?) => #f)

; Error handling tests
(check-catch 'wrong-type-arg (string-index 123 #\a))
(check-catch 'wrong-type-arg (string-index "hello" "a"))
(check-catch 'wrong-type-arg (string-index "hello" 123))
(check-catch 'wrong-type-arg (string-index "hello" '(a)))
(check-catch 'out-of-range (string-index "hello" #\a -1))
(check-catch 'out-of-range (string-index "hello" #\a 0 6))
(check-catch 'out-of-range (string-index "hello" #\a 3 2))
(check-catch 'out-of-range (string-index "" #\a 1))
(check-catch 'out-of-range (string-index "abc" #\a 5))

(check-report)
