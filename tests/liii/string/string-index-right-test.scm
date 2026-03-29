(import (liii check)
        (liii string))

;; string-index-right
;; 在字符串中从右向左查找指定字符或满足条件的第一个字符的位置。
;;
;; 语法
;; ----
;; (string-index-right str char/pred?)
;; (string-index-right str char/pred? start)
;; (string-index-right str char/pred? start end)
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
;; string-index-right从字符串的右侧(末尾)开始搜索，返回第一个匹配字符的索引位置。
;; 搜索范围由start和end参数限定。空字符串或未找到匹配项时返回#f。
;;
;; 该函数支持使用字符和谓词两种方式进行查找:
;; - 字符匹配：查找与指定字符相等的字符
;; - 谓词匹配：查找使谓词返回#t的第一个字符
;;
;; 与string-index的主要区别是搜索方向：string-index从左向右搜索，string-index-right从右向左搜索。
;;
;; 示例
;; ----
;; (string-index-right "hello" #\l) => 3  (从右向左第一个'l'在索引3处)
;; (string-index-right "hello" #\z) => #f (没有找到字符'z')
;; (string-index-right "abc123" char-numeric?) => 5 (最后一个数字'3'在索引5处)
;; (string-index-right "hello" char-alphabetic?) => 4 (最后一个字母'o'在索引4处)
;; (string-index-right "hello" #\l 0 4) => 2 (在0到4范围内从右向左找字符'l')
;; (string-index-right "" #\x) => #f (空字符串返回#f)
;;
;; 错误处理
;; ----
;; wrong-type-arg 当str不是字符串类型时
;; wrong-type-arg 当char/pred?不是字符或谓词时
;; out-of-range 当start/end超出字符串索引范围时

; Basic functionality tests
(check (string-index-right "hello" #\l) => 3)
(check (string-index-right "hello" #\z) => #f)
(check (string-index-right "hello" #\l) => 3)
(check (string-index-right "hello" #\l 0 3) => 2)
(check (string-index-right "abc123" char-numeric?) => 5)
(check (string-index-right "abc123" char-alphabetic?) => 2)
(check (string-index-right "" #\x) => #f)

; Character parameter tests
(check (string-index-right "0123456789" #\2) => 2)
(check (string-index-right "0123456789" #\2 0 3) => 2)
(check (string-index-right "0123456789" #\2 0 2) => #f)
(check (string-index-right "abccba" #\a) => 5)
(check (string-index-right "hello world" #\space) => 5)

; Extended comprehensive tests
(check (string-index-right "hello" #\h) => 0)
(check (string-index-right "hello" #\o) => 4)
(check (string-index-right "hello hello" #\space) => 5)
(check (string-index-right "hello" #\H) => #f) ; case-sensitive
(check (string-index-right "" #\a) => #f)
(check (string-index-right "a" #\a) => 0)
(check (string-index-right "aaaa" #\a) => 3)
(check (string-index-right "0123456789" #\0) => 0)
(check (string-index-right "0123456789" #\9) => 9)

; Predicate parameter tests
(check (string-index-right "0123456789" char-numeric?) => 9)
(check (string-index-right "abc123" char-numeric?) => 5)
(check (string-index-right "123abc" char-alphabetic?) => 5)
(check (string-index-right "Hello123" char-upper-case?) => 0)
(check (string-index-right "hello123" char-upper-case?) => #f)
(check (string-index-right "123!@#" char-alphabetic?) => #f)
(check (string-index-right "hello\n\t " char-whitespace?) => 7)
(check (string-index-right "hello" (lambda (c) (char=? c #\l))) => 3)

; Single character edge cases
(check (string-index-right "a" #\a) => 0)
(check (string-index-right "a" #\b) => #f)
(check (string-index-right " " #\space) => 0)
(check (string-index-right "\t" char-whitespace?) => 0)

; Start and end parameter tests
(check (string-index-right "hello" #\l 0) => 3)
(check (string-index-right "hello" #\l 1) => 3)
(check (string-index-right "hello" #\l 2) => 3)
(check (string-index-right "hello" #\l 3) => 3)
(check (string-index-right "hello" #\l 4) => #f)
(check (string-index-right "hello" #\l 0 3) => 2)
(check (string-index-right "hello" #\l 0 2) => #f)
(check (string-index-right "hello" #\l 1 4) => 3)
(check (string-index-right "hello" #\l 2 4) => 3)
(check (string-index-right "hello" #\l 3 4) => 3)
(check (string-index-right "hello" #\l 3 3) => #f)
(check (string-index-right "hello" #\l 0 1) => #f)

; Special characters and edge cases
(check (string-index-right "_test" #\_) => 0)
(check (string-index-right "a@b" #\@) => 1)
(check (string-index-right "hello,world" #\,) => 5)
(check (string-index-right "a-b-c" #\-) => 3)

; Complex predicates
(check (string-index-right "123abc!@#" (lambda (c) (or (char-alphabetic? c) (char-numeric? c)))) => 5)
(check (string-index-right "!@#abc123" (lambda (c) (or (char-alphabetic? c) (char-numeric? c)))) => 8)
(check (string-index-right "abc123" char-upper-case?) => #f)
(check (string-index-right "ABC123" char-upper-case?) => 2)
(check (string-index-right "abcABC" char-upper-case?) => 5)

; Error handling tests
(check-catch 'wrong-type-arg (string-index-right 123 #\a))
(check-catch 'wrong-type-arg (string-index-right "hello" "a"))
(check-catch 'wrong-type-arg (string-index-right "hello" 123))
(check-catch 'wrong-type-arg (string-index-right "hello" '(a)))
(check-catch 'out-of-range (string-index-right "hello" #\a -1))
(check-catch 'out-of-range (string-index-right "hello" #\a 0 6))
(check-catch 'out-of-range (string-index-right "hello" #\a 3 2))
(check-catch 'out-of-range (string-index-right "" #\a 1))
(check-catch 'out-of-range (string-index-right "abc" #\a 5))

(check-report)
