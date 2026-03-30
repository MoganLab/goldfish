(import (liii check)
        (srfi srfi-13)
) ;import

;; string-prefix?
;; 检查字符串是否以指定前缀开始。
;;
;; 语法
;; ----
;; (string-prefix? prefix str)
;;
;; 参数
;; ----
;; prefix : string?
;; 要检查的前缀字符串。
;;
;; str : string?
;; 要检查的源字符串。
;;
;; 返回值
;; ----
;; boolean
;; 如果str以prefix开头则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 字符串前缀匹配是指检查指定的前缀字符串是否与源字符串的开头完全一致。
;; 符合SRFI-13标准规范的字符串前缀检查功能。
;;
;; 空字符串作为prefix时总是返回#t，因为任何字符串都以空字符串开始。
;; 当prefix长度大于源字符串长度时，string-prefix?返回#f。
;; 该函数区分大小写，"Hello"不会匹配"hello"作为前缀。
;;
;; string-prefix?支持Unicode多字节字符，包括中文、日文、emoji等Unicode字符。
;; 对于多字节字符，操作按字符逻辑进行而非字节级操作，确保Unicode字符被正确处理。
;;
;;
;; 错误处理
;; ----
;; type-error 当任一参数不是字符串类型时抛出。

; string-prefix? 基本功能验证测试
(check (string-prefix? "" "hello") => #t)
(check (string-prefix? "h" "hello") => #t)
(check (string-prefix? "he" "hello") => #t)
(check (string-prefix? "hel" "hello") => #t)
(check (string-prefix? "hell" "hello") => #t)
(check (string-prefix? "hello" "hello") => #t)
(check (string-prefix? "test" "test123") => #t)
(check (string-prefix? "" "") => #t)
(check (string-prefix? "a" "a") => #t)
(check (string-prefix? "abc" "abc") => #t)

; 边界条件和特殊情况测试
(check (string-prefix? "a" "ab") => #t)
(check (string-prefix? "" "a") => #t)
(check (string-prefix? "" "") => #t)
(check (string-prefix? "a" "a") => #t)
(check (string-prefix? "abc" "ab") => #f)
(check (string-prefix? "long-prefix-long" "short") => #f)

; 复杂场景和Unicode支持测试
(check (string-prefix? "中" "中文") => #t)
(check (string-prefix? "中文" "中文测试") => #t)
(check (string-prefix? "uni" "unicode") => #t)
(check (string-prefix? "🌟" "🌟🎉") => #t)
(check (string-prefix? "中文123" "中文123abc") => #t)
(check (string-prefix? "测试多功能" "测试多功能边界处理") => #t)

; 字符串与自身关系测试
(check (string-prefix? "hello" "hello") => #t)
(check (string-prefix? "world" "world") => #t)
(check (string-prefix? "完整测试" "完整测试") => #t)

; 空字符串作为字符串参数测试
(check (string-prefix? "" "") => #t)
(check (string-prefix? "a" "") => #f)
(check (string-prefix? "hello" "") => #f)

; 长前缀与短字符串对比测试
(check (string-prefix? "prefix-is-longer-than-string" "short") => #f)
(check (string-prefix? "university" "uni") => #f)
(check (string-prefix? "test" "testing") => #t)

; 大小写敏感验证测试
(check (string-prefix? "Hello" "hello") => #f)
(check (string-prefix? "hello" "Hello") => #f)
(check (string-prefix? "TEST" "test") => #f)
(check (string-prefix? "大写" "大写") => #t)
(check (string-prefix? "大" "大写") => #t)

; 特殊字符模式测试
(check (string-prefix? "_hidden" "_hidden_file") => #t)
(check (string-prefix? "./path" "./path/to/file") => #t)
(check (string-prefix? " multiple spaces" " multiple spaces ahead") => #t)

; 哨兵值和边界值测试
(check (string-prefix? "" "single-char") => #t)
(check (string-prefix? "🙂" "🙂") => #t)
(check (string-prefix? "a⚡b" "a⚡btest") => #t)

; 错误处理 - 类型验证
(check-catch 'wrong-type-arg (string-prefix? 123 "hello"))
(check-catch 'wrong-type-arg (string-prefix? "hello" 123))
(check-catch 'wrong-type-arg (string-prefix? '(a b c) "hello"))
(check-catch 'wrong-type-arg (string-prefix? "hello" #\c))
(check-catch 'wrong-type-arg (string-prefix? "hello" 'symbol))
(check-catch 'wrong-type-arg (string-prefix? '() "hello"))

(check-report)
