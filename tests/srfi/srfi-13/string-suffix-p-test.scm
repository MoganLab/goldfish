(import (liii check)
        (srfi srfi-13))

;; string-suffix?
;; 检查字符串是否以指定后缀结束。
;;
;; 语法
;; ----
;; (string-suffix? suffix str)
;;
;; 参数
;; ----
;; suffix : string?
;; 要检查的后缀字符串。
;;
;; str : string?
;; 要检查的源字符串。
;;
;; 返回值
;; ----
;; boolean
;; 如果str以suffix结尾则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 字符串后缀匹配是指检查指定的后缀字符串是否与源字符串的末尾完全一致。
;; 符合SRFI-13标准规范的字符串后缀检查功能。
;;
;; 建议使用 `string-ends?` 函数代替 `string-suffix?`。
;; `string-ends?` 提供更友好的函数签名和更好的用户体验。
;;
;; 空字符串作为suffix时总是返回#t，因为任何字符串都以空字符串结束。
;; 当suffix长度大于源字符串长度时，string-suffix?返回#f。
;; 该函数区分大小写，"Test"不会匹配"test"作为后缀。
;;
;; string-suffix?支持Unicode多字节字符，包括中文、日文、emoji等Unicode字符。
;; 对于多字节字符，操作按字符逻辑进行而非字节级操作，确保Unicode字符被正确处理。
;;
;; 错误处理
;; ----
;; type-error 当任一参数不是字符串类型时抛出。

; string-suffix? 基本功能验证测试
(check (string-suffix? "" "hello") => #t)
(check (string-suffix? "o" "hello") => #t)
(check (string-suffix? "lo" "hello") => #t)
(check (string-suffix? "llo" "hello") => #t)
(check (string-suffix? "ello" "hello") => #t)
(check (string-suffix? "hello" "hello") => #t)
(check (string-suffix? "123" "test123") => #t)
(check (string-suffix? "" "") => #t)
(check (string-suffix? "a" "a") => #t)
(check (string-suffix? "abc" "abc") => #t)

; 边界条件和特殊情况测试
(check (string-suffix? "b" "ab") => #t)
(check (string-suffix? "" "a") => #t)
(check (string-suffix? "" "") => #t)
(check (string-suffix? "a" "a") => #t)
(check (string-suffix? "ab" "a") => #f)
(check (string-suffix? "short-right" "long-suffix-long") => #f)

; 复杂场景和Unicode支持测试
(check (string-suffix? "文" "中文") => #t)
(check (string-suffix? "测试" "中文测试") => #t)
(check (string-suffix? "code" "unicode") => #t)
(check (string-suffix? "🎉" "🌟🎉") => #t)
(check (string-suffix? "123abc" "中文123abc") => #t)
(check (string-suffix? "边界处理" "测试多功能边界处理") => #t)

; 字符串与自身关系测试
(check (string-suffix? "hello" "hello") => #t)
(check (string-suffix? "world" "world") => #t)
(check (string-suffix? "完整测试" "完整测试") => #t)

; 空字符串作为字符串参数测试
(check (string-suffix? "" "") => #t)
(check (string-suffix? "a" "") => #f)
(check (string-suffix? "hello" "") => #f)

; 长后缀与短字符串对比测试
(check (string-suffix? "longer-than-original" "short") => #f)
(check (string-suffix? "versity" "university") => #t)
(check (string-suffix? "ing" "testing") => #t)

; 大小写敏感验证测试
(check (string-suffix? "Test" "hello Test") => #t)
(check (string-suffix? "test" "hello Test") => #f)
(check (string-suffix? "TEST" "test") => #f)
(check (string-suffix? "大写" "测试中文字符大写") => #t)
(check (string-suffix? "小" "全部字符小") => #t)

; 特殊字符和模式测试
(check (string-suffix? "_file" "_hidden_file") => #t)
(check (string-suffix? "/path" "filedir/path") => #t)
(check (string-suffix? " multiple" "with multiple spaces multiple") => #t)

; 哨兵值和边界值测试
(check (string-suffix? "" "single-char") => #t)
(check (string-suffix? "🙂" "🙂") => #t)
(check (string-suffix? "b⚡c" "testb⚡c") => #t)

; 文件扩展名模拟测试
(check (string-suffix? ".txt" "document.txt") => #t)
(check (string-suffix? ".json" "data.json") => #t)
(check (string-suffix? ".tmu" "report.tmu") => #t)
(check (string-suffix? "backup.txt" "file.backup.txt") => #t)

; 错误处理 - 类型验证
(check-catch 'wrong-type-arg (string-suffix? 123 "hello"))
(check-catch 'wrong-type-arg (string-suffix? "hello" 123))
(check-catch 'wrong-type-arg (string-suffix? '(a b c) "hello"))
(check-catch 'wrong-type-arg (string-suffix? "hello" #\c))
(check-catch 'wrong-type-arg (string-suffix? "hello" 'symbol))
(check-catch 'wrong-type-arg (string-suffix? '() "hello"))

(check-report)
