(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; string-length
;; 返回给定字符串的 Unicode 字符数。
;;
;; 语法
;; ----
;; (string-length string)
;;
;; 参数
;; ----
;; string : string?
;; 要测量长度的字符串，可以是空字符串、单字符字符串或多字符字符串。
;;
;; 返回值
;; ------
;; integer?
;; 返回一个非负整数，表示字符串的字符数。
;;
;; 说明
;; ----
;; 1. 字符串长度包含所有字符，包括空格、制表符和换行符
;; 2. 空字符串 "" 的长度为 0
;; 3. 对于ASCII字符（0-127），每个字符占用1字节
;; 4. 非ASCII字符也各计为一个字符
;; 5. 字符串不会改变原始数据，只是返回长度信息
;;
;; 边界情况
;; --------
;; - 空字符串长度：0
;; - ASCII和非ASCII字符均计为一个字符
;; - UTF-8字节数由 (bytevector-length (string->utf8 str)) 获取
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 当参数不是字符串类型时抛出错误。
;; wrong-number-of-args
;; 当参数数量不为1个时抛出错误。
;; string-length 基础测试
(check (string-length "") => 0)
(check (string-length "a") => 1)
(check (string-length "hello") => 5)
(check (string-length "世界") => 2)
(check (string-length "你好世界") => 4)
;; 空字符串测试
(check (string-length "") => 0)
(check (string-length (list->string '())) => 0)
;; 单字符测试
(check (string-length "a") => 1)
(check (string-length "A") => 1)
(check (string-length "1") => 1)
(check (string-length "!") => 1)
(check (string-length " ") => 1)
;; 多字符测试
(check (string-length "abc") => 3)
(check (string-length "ABC") => 3)
(check (string-length "123") => 3)
(check (string-length "!@#") => 3)
;; 含有空格的字符串
(check (string-length "hello world") => 11)
(check (string-length "  ") => 2)
(check (string-length " leading space") => 14)
(check (string-length "trailing space ") => 15)
;; 特殊字符和空白字符
(check (string-length "hello\nworld") => 11)
(check (string-length "tab\tseparated") => 13)
(check (string-length "line\rreturn") => 11)
;; Unicode字符测试
(check (string-length "😀") => 1)
(check (string-length "μ") => 1)
(check (string-length "ä") => 1)
(check (string-length "中文") => 2)
;; 长度边界测试
(check (string-length "a") => 1)
(check (string-length "abcdefghijklmnop") => 16)
(check (string-length "abcdefghijklmnopqrstuvwxyz") => 26)
(check (string-length "aaaaaaaaaaaaaaaaaaaaaaaaaa") => 26)
;; 与字符串生成函数的兼容性测试
(check (string-length (make-string 5 #\a)) => 5)
(check (string-length (make-string 10 #\x)) => 10)
(check (string-length (make-string 0)) => 0)
;; 与字符串拼接函数的兼容性测试
(check (string-length (string-append "hello" "world")) => 10)
(check (string-length (string-append "" "")) => 0)
(check (string-length (string-append "a" "b")) => 2)
;; 错误处理测试
(check-catch 'wrong-type-arg (string-length 123))
(check-catch 'wrong-type-arg (string-length 'symbol))
(check-catch 'wrong-type-arg (string-length #t))
(check-catch 'wrong-type-arg (string-length '()))
(check-catch 'wrong-type-arg (string-length #(1 2 3)))
(check-catch 'wrong-number-of-args (string-length))
(check-catch 'wrong-number-of-args (string-length "hello" "world"))
(check-catch 'wrong-number-of-args (string-length "hello" 1))
(check-report)
