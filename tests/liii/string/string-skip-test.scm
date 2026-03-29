(import (liii check)
        (liii string)
) ;import

;; string-skip
;; 在字符串中从左向右跳过指定字符或满足条件的字符，返回第一个不满足条件的字符位置。
;;
;; 语法
;; ----
;; (string-skip str char/pred?)
;; (string-skip str char/pred? start)
;; (string-skip str char/pred? start end)
;;
;; 参数
;; ----
;; str : string?
;; 要搜索的源字符串。
;;
;; char/pred? : char? 或 procedure?
;; - 字符(char)：要跳过的目标字符
;; - 谓词(procedure)：接受单个字符作为参数的函数，返回布尔值指示是否跳过该字符
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
;; - 如果找到不匹配的字符，返回其索引位置(从0开始计数)
;; - 如果所有字符都匹配（都满足跳过条件），返回#f
;;
;; 注意
;; ----
;; string-skip从字符串的左侧(开头)开始搜索，跳过所有满足条件的字符，返回第一个不满足条件的字符索引。
;; 搜索范围由start和end参数限定。如果指定范围内的所有字符都满足跳过条件，则返回#f。
;;
;; 该函数支持使用字符和谓词两种方式:
;; - 字符匹配：跳过与指定字符相等的字符
;; - 谓词匹配：跳过使谓词返回#t的字符
;;
;; string-skip是string-index的补充：string-index查找满足条件的字符，string-skip查找不满足条件的字符。
;;
;; 错误处理
;; ----
;; wrong-type-arg 当str不是字符串类型时
;; wrong-type-arg 当char/pred?不是字符或谓词时
;; out-of-range 当start/end超出字符串索引范围时

;; 基本功能测试
(check (string-skip "   hello" #\space) => 3)
(check (string-skip "aaaa" #\a) => #f)
(check (string-skip "123abc" char-numeric?) => 3)
(check (string-skip "abc123" char-alphabetic?) => 3)
(check (string-skip "" #\space) => #f)

;; 字符参数测试
(check (string-skip "xxxabc" #\x) => 3)
(check (string-skip "xxxabc" #\x 2) => 3)
(check (string-skip "xxxabc" #\x 4) => 4)
(check (string-skip "   \t\n  " char-whitespace?) => #f)

;; 扩展综合测试
(check (string-skip "hello" #\h) => 1)
(check (string-skip "hhhello" #\h) => 3)
(check (string-skip "hhh" #\h) => #f)
(check (string-skip "hello world" #\h) => 1)
(check (string-skip "hello" #\x) => 0)
(check (string-skip "" #\a) => #f)
(check (string-skip "a" #\a) => #f)
(check (string-skip "a" #\b) => 0)
(check (string-skip "0123456789" #\0) => 1)
(check (string-skip "0123456789" #\1) => 0)

;; 谓词参数测试
(check (string-skip "0123456789" char-numeric?) => #f)
(check (string-skip "abc123" char-numeric?) => 0)
(check (string-skip "123abc" char-alphabetic?) => 0)
(check (string-skip "Hello123" char-upper-case?) => 1)
(check (string-skip "HELLO" char-upper-case?) => #f)
(check (string-skip "123!@#" char-alphabetic?) => 0)
(check (string-skip "   hello" char-whitespace?) => 3)
(check (string-skip "hello" (lambda (c) (char=? c #\h))) => 1)

;; 单字符边界情况
(check (string-skip "a" #\a) => #f)
(check (string-skip "a" #\b) => 0)
(check (string-skip " " #\space) => #f)
(check (string-skip "\t" char-whitespace?) => #f)

;; start和end参数测试
(check (string-skip "xxxabc" #\x 0) => 3)
(check (string-skip "xxxabc" #\x 1) => 3)
(check (string-skip "xxxabc" #\x 2) => 3)
(check (string-skip "xxxabc" #\x 3) => 3)
(check (string-skip "xxxabc" #\x 4) => 4)
(check (string-skip "xxxabc" #\x 0 3) => #f)
(check (string-skip "xxxabc" #\x 0 4) => 3)
(check (string-skip "xxxabc" #\x 1 4) => 3)
(check (string-skip "xxxabc" #\x 2 4) => 3)
(check (string-skip "xxxabc" #\x 3 4) => 3)
(check (string-skip "xxxabc" #\x 3 3) => #f)

;; 特殊字符和边界情况
(check (string-skip "___test" #\_) => 3)
(check (string-skip "a@@b" #\@) => 0)
(check (string-skip "---a" #\-) => 3)
(check (string-skip ",,hello" #\,) => 2)

;; 复杂谓词
(check (string-skip "123abc!@#" (lambda (c) (or (char-alphabetic? c) (char-numeric? c)))) => 6)
(check (string-skip "   abc123" char-whitespace?) => 3)
(check (string-skip "abc123" char-upper-case?) => 0)
(check (string-skip "ABC123" char-upper-case?) => 3)
(check (string-skip "ABCabc" char-upper-case?) => 3)

;; 错误处理测试
(check-catch 'wrong-type-arg (string-skip 123 #\a))
(check-catch 'wrong-type-arg (string-skip "hello" "a"))
(check-catch 'wrong-type-arg (string-skip "hello" 123))
(check-catch 'wrong-type-arg (string-skip "hello" '(a)))
(check-catch 'out-of-range (string-skip "hello" #\a -1))
(check-catch 'out-of-range (string-skip "hello" #\a 0 6))
(check-catch 'out-of-range (string-skip "hello" #\a 3 2))
(check-catch 'out-of-range (string-skip "" #\a 1))
(check-catch 'out-of-range (string-skip "abc" #\a 5))

;; string-skip-right
;; 在字符串中从右向左跳过指定字符或满足条件的字符，返回第一个不满足条件的字符位置。
;;
;; 语法
;; ----
;; (string-skip-right str char/pred?)
;; (string-skip-right str char/pred? start)
;; (string-skip-right str char/pred? start end)
;;
;; 参数
;; ----
;; str : string?
;; 要搜索的源字符串。
;;
;; char/pred? : char? 或 procedure?
;; - 字符(char)：要跳过的目标字符
;; - 谓词(procedure)：接受单个字符作为参数的函数，返回布尔值指示是否跳过该字符
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
;; - 如果找到不匹配的字符，返回其索引位置(从0开始计数)
;; - 如果所有字符都匹配（都满足跳过条件），返回#f
;;
;; 注意
;; ----
;; string-skip-right从字符串的右侧(末尾)开始搜索，跳过所有满足条件的字符，返回第一个不满足条件的字符索引。
;; 搜索范围由start和end参数限定。如果指定范围内的所有字符都满足跳过条件，则返回#f。
;;
;; 该函数支持使用字符和谓词两种方式:
;; - 字符匹配：跳过与指定字符相等的字符
;; - 谓词匹配：跳过使谓词返回#t的字符
;;
;; string-skip-right是string-index-right的补充：string-index-right查找满足条件的字符，string-skip-right查找不满足条件的字符。
;;
;; 错误处理
;; ----
;; wrong-type-arg 当str不是字符串类型时
;; wrong-type-arg 当char/pred?不是字符或谓词时
;; out-of-range 当start/end超出字符串索引范围时

;; 基本功能测试
(check (string-skip-right "hello   " #\space) => 4)
(check (string-skip-right "aaaa" #\a) => #f)
(check (string-skip-right "abc123" char-numeric?) => 2)
(check (string-skip-right "123abc" char-alphabetic?) => 2)
(check (string-skip-right "" #\space) => #f)

;; 字符参数测试
(check (string-skip-right "abcxxx" #\x) => 2)
(check (string-skip-right "abcxxx" #\x 0 5) => 2)
(check (string-skip-right "abcxxx" #\x 0 4) => 2)
(check (string-skip-right "   \t\n  " char-whitespace?) => #f)

;; 扩展综合测试
(check (string-skip-right "helloh" #\h) => 4)
(check (string-skip-right "hellohh" #\h) => 4)
(check (string-skip-right "hhh" #\h) => #f)
(check (string-skip-right "hello world" #\d) => 9)
(check (string-skip-right "hello" #\x) => 4)
(check (string-skip-right "" #\a) => #f)
(check (string-skip-right "a" #\a) => #f)
(check (string-skip-right "a" #\b) => 0)
(check (string-skip-right "0123456789" #\9) => 8)
(check (string-skip-right "0123456789" #\8) => 9)

;; 谓词参数测试
(check (string-skip-right "0123456789" char-numeric?) => #f)
(check (string-skip-right "abc123" char-numeric?) => 2)
(check (string-skip-right "123abc" char-alphabetic?) => 2)
(check (string-skip-right "Hello123" char-upper-case?) => 7)
(check (string-skip-right "HELLO" char-upper-case?) => #f)
(check (string-skip-right "123!@#" char-alphabetic?) => 5)
(check (string-skip-right "hello   " char-whitespace?) => 4)
(check (string-skip-right "helloh" (lambda (c) (char=? c #\h))) => 4)

;; 单字符边界情况
(check (string-skip-right "a" #\a) => #f)
(check (string-skip-right "a" #\b) => 0)
(check (string-skip-right " " #\space) => #f)
(check (string-skip-right "\t" char-whitespace?) => #f)

;; start和end参数测试
(check (string-skip-right "abcxxx" #\x 0) => 2)
(check (string-skip-right "abcxxx" #\x 1) => 2)
(check (string-skip-right "abcxxx" #\x 2) => 2)
(check (string-skip-right "abcxxx" #\x 3) => #f)
(check (string-skip-right "abcxxx" #\x 4) => #f)
(check (string-skip-right "abcxxx" #\x 0 3) => 2)
(check (string-skip-right "abcxxx" #\x 0 4) => 2)
(check (string-skip-right "abcxxx" #\x 1 4) => 2)
(check (string-skip-right "abcxxx" #\x 2 4) => 2)
(check (string-skip-right "abcxxx" #\x 3 4) => #f)
(check (string-skip-right "abcxxx" #\x 3 3) => #f)

;; 特殊字符和边界情况
(check (string-skip-right "test___" #\_) => 3)
(check (string-skip-right "b@@a" #\@) => 3)
(check (string-skip-right "a---" #\-) => 0)
(check (string-skip-right "hello,," #\,) => 4)

;; 复杂谓词
(check (string-skip-right "!@#abc123" (lambda (c) (or (char-alphabetic? c) (char-numeric? c)))) => 2)
(check (string-skip-right "abc123   " char-whitespace?) => 5)
(check (string-skip-right "abc123" char-upper-case?) => 5)
(check (string-skip-right "ABC123" char-upper-case?) => 5)
(check (string-skip-right "abcABC" char-upper-case?) => 2)

;; 错误处理测试
(check-catch 'wrong-type-arg (string-skip-right 123 #\a))
(check-catch 'wrong-type-arg (string-skip-right "hello" "a"))
(check-catch 'wrong-type-arg (string-skip-right "hello" 123))
(check-catch 'wrong-type-arg (string-skip-right "hello" '(a)))
(check-catch 'out-of-range (string-skip-right "hello" #\a -1))
(check-catch 'out-of-range (string-skip-right "hello" #\a 0 6))
(check-catch 'out-of-range (string-skip-right "hello" #\a 3 2))
(check-catch 'out-of-range (string-skip-right "" #\a 1))
(check-catch 'out-of-range (string-skip-right "abc" #\a 5))

(check-report)
