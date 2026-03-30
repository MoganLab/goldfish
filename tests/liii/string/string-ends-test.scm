(import (liii check)
        (liii string))

;; string-ends?
;; 检查字符串是否以指定后缀结束。
;;
;; 语法
;; ----
;; (string-ends? str suffix)
;;
;; 参数
;; ----
;; str : string?
;; 要检查的源字符串。
;;
;; suffix : string?
;; 后缀字符串，用于检查str是否以其结束。
;;
;; 返回值
;; ----
;; boolean
;; 如果str以suffix结尾返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 该函数默认使用标准SRFI-13中的string-suffix?实现。
;; 空字符串作为suffix时总是返回#t，因为任何字符串都以空字符串结束。
;; 当suffix长度大于str长度时，string-ends?返回#f。
;;
;; 错误处理
;; ----
;; type-error 当参数不是字符串类型时。需要两个参数都是字符串；非字符串参数会抛出type-error。

; Basic functionality tests
(check-true (string-ends? "MathAgape" "e"))
(check-true (string-ends? "MathAgape" ""))
(check-true (string-ends? "MathAgape" "MathAgape"))

; Single character suffix testing
(check-true (string-ends? "hello" "o"))
(check-true (string-ends? "world" "d"))
(check-true (string-ends? "测试" "试"))
(check-false (string-ends? "hello" "x"))

; Multi-character suffix testing
(check-true (string-ends? "hello world" "world"))
(check-true (string-ends? "greeting" "ing"))
(check-true (string-ends? "national" "onal"))
(check-true (string-ends? "filename" "name"))
(check-false (string-ends? "hello" "test"))

; Exact string matching
(check-true (string-ends? "identical" "identical"))
(check-true (string-ends? "hello" "hello"))
(check-true (string-ends? "中文测试" "中文测试"))

; Empty string edge cases
(check-true (string-ends? "" ""))
(check-true (string-ends? "non-empty" ""))
(check-false (string-ends? "" "non-empty"))

; Length boundary testing
(check-false (string-ends? "hi" "hello"))    ; suffix longer than string
(check-false (string-ends? "short" "longer"))
(check-true (string-ends? "longer" "er"))
(check-true (string-ends? "a" "a"))
(check-false (string-ends? "a" "ab"))

; Case sensitivity testing
(check-true (string-ends? "HelloWorld" "World"))
(check-false (string-ends? "HelloWorld" "world"))
(check-true (string-ends? "TestCase" "Case"))
(check-false (string-ends? "TestCase" "case"))

; File extension testing (real scenarios)
(check-true (string-ends? "document.txt" ".txt"))
(check-true (string-ends? "report.pdf" ".pdf"))
(check-true (string-ends? "config.json" ".json"))
(check-true (string-ends? "image.jpeg" ".jpeg"))
(check-false (string-ends? "document.txt" ".pdf"))
(check-false (string-ends? "noextension" ".txt"))

; Version number testing
(check-true (string-ends? "app-v1.0.0" "1.0.0"))
(check-true (string-ends? "release-alpha" "-alpha"))
(check-true (string-ends? "build-SNAPSHOT" "SNAPSHOT"))
(check-true (string-ends? "product-beta" "-beta"))

; URL path testing
(check-true (string-ends? "/api/v1/users" "users"))
(check-true (string-ends? "/index.html" ".html"))
(check-true (string-ends? "/api/endpoint/" "/"))
(check-false (string-ends? "/api/users" "admin"))

; Programming identifier testing
(check-true (string-ends? "DatabaseImpl" "Impl"))
(check-true (string-ends? "UserService" "Service"))
(check-true (string-ends? "DataMapper" "Mapper"))
(check-true (string-ends? "FileHandler" "Handler"))
(check-false (string-ends? "SimpleClass" "Utils"))

; Unicode comprehensive testing
(check-true (string-ends? "中文测试" "测试"))
(check-true (string-ends? "文件名" "名"))
(check-true (string-ends? "项目说明" "说明"))
(check-true (string-ends? "emoji测试" "测试"))
(check-false (string-ends? "中文文件" "测试"))

; Mixed Unicode scenarios
(check-true (string-ends? "文件🌟txt" "txt"))
(check-true (string-ends? "配置📄json" "json"))
(check-true (string-ends? "测试✅中文" "中文"))
(check-true (string-ends? "混合😀表情" "表情"))

; Emoji testing
(check-true (string-ends? "Hello😀" "😀"))
(check-true (string-ends? "Star ⭐" "⭐"))
(check-true (string-ends? "表情😂😃" "😃"))
(check-false (string-ends? "Hello😀" "😂"))

; Multi-byte character combinations
(check-true (string-ends? "测试中文编程" "编程"))
(check-true (string-ends? "Japanese文字日本語" "日本語"))
(check-true (string-ends? "Korean한국어" "한국어"))
(check-true (string-ends? "数学方程式equation" "equation"))

; Complex strings with special characters
(check-true (string-ends? "config-file-name" "name"))
(check-true (string-ends? "user_name_123" "123"))
(check-true (string-ends? "file-name_ver2.0" "2.0"))
(check-false (string-ends? "config-file" "name"))

; Format detection scenarios
(check-true (string-ends? "data.csv" ".csv"))
(check-true (string-ends? "backup.sql" ".sql"))
(check-true (string-ends? "archive.zip" ".zip"))
(check-true (string-ends? "logfile.log" ".log"))
(check-true (string-ends? "script.py" ".py"))

; Offset testing
(check-true (string-ends? "1" "1"))
(check-true (string-ends? "12" "2"))
(check-true (string-ends? "123" "3"))
(check-true (string-ends? "1234" "4"))
(check-false (string-ends? "123" "xyz"))

; Length edge cases
(check-true (string-ends? "a" "a"))
(check-true (string-ends? "ab" "b"))
(check-true (string-ends? "abc" "c"))
(check-false (string-ends? "a" "ab"))
(check-false (string-ends? "ab" "abc"))

; Real-world template matching
(check-true (string-ends? "CustomerData.java" ".java"))
(check-true (string-ends? "UserRepositoryImpl" "Impl"))
(check-true (string-ends? "api_response.json" ".json"))
(check-true (string-ends? "daily_report_2023-08-08.csv" ".csv"))

; Mathematics and symbols
(check-true (string-ends? "equation=x+y+z" "z"))
(check-true (string-ends? "math_pi=3.14159" "14159"))
(check-true (string-ends? "temperature_25°C" "°C"))
(check-false (string-ends? "formula=area" "volume"))

; Web development context
(check-true (string-ends? "index.min.js" ".js"))
(check-true (string-ends? "styles.css.map" ".map"))
(check-true (string-ends? "bundle.js.gz" ".gz"))
(check-true (string-ends? "app.d.ts" ".ts"))

; Documentation suffixes
(check-true (string-ends? "README.md" ".md"))
(check-true (string-ends? "CHANGELOG.rst" ".rst"))
(check-true (string-ends? "LICENSE.txt" ".txt"))
(check-true (string-ends? "Makefile" "file"))

; Date/time formatting
(check-true (string-ends? "backup_20230808" "08"))
(check-true (string-ends? "log_2023-08-08_15:30:00" ":00"))
(check-true (string-ends? "event_20230808T153000Z" "000Z"))

; Error handling tests - type-error validation
(check-catch 'type-error (string-ends? 123 "test"))
(check-catch 'type-error (string-ends? "test" #f))
(check-catch 'type-error (string-ends? #t "suffix"))
(check-catch 'type-error (string-ends? 'symbol "test"))
(check-catch 'type-error (string-ends? "hello" 456))
(check-catch 'type-error (string-ends? 'name "test"))
(check-catch 'type-error (string-ends? 123 456))
(check-catch 'type-error (string-ends? "test" 'invalid))
(check-catch 'type-error (string-ends? #f #t))
(check-catch 'type-error (string-ends? '() "test"))
(check-catch 'type-error (string-ends? "hello" '()))

; Special numerical edge cases for error handling
(check-catch 'type-error (string-ends? 0 "suffix"))
(check-catch 'type-error (string-ends? "" 0))
(check-catch 'type-error (string-ends? 1.5 "test"))
(check-catch 'type-error (string-ends? "string" 2.0))

; List and vector error cases
(check-catch 'type-error (string-ends? '(1 2 3) "test"))
(check-catch 'type-error (string-ends? "test" '(1 2 3)))
(check-catch 'type-error (string-ends? 999 "test"))
(check-catch 'type-error (string-ends? "valid" 888))

(check-report)
