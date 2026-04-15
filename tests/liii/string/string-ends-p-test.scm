(import (liii check) (liii string))

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

(check-true (string-ends? "MathAgape" "e")
) ;check-true
(check-true (string-ends? "MathAgape" "")
) ;check-true
(check-true (string-ends? "MathAgape" "MathAgape")
) ;check-true

(check-true (string-ends? "hello" "o"))
(check-true (string-ends? "world" "d"))
(check-true (string-ends? "测试" "试")
) ;check-true
(check-false (string-ends? "hello" "x"))

(check-true (string-ends? "hello world" "world")
) ;check-true
(check-true (string-ends? "greeting" "ing")
) ;check-true
(check-true (string-ends? "national" "onal")
) ;check-true
(check-true (string-ends? "filename" "name")
) ;check-true
(check-false (string-ends? "hello" "test")
) ;check-false

(check-true (string-ends? "identical" "identical")
) ;check-true
(check-true (string-ends? "hello" "hello")
) ;check-true
(check-true (string-ends? "中文测试"
              "中文测试"
            ) ;string-ends?
) ;check-true

(check-true (string-ends? "" ""))
(check-true (string-ends? "non-empty" "")
) ;check-true
(check-false (string-ends? "" "non-empty")
) ;check-false

(check-false (string-ends? "hi" "hello")
) ;check-false
(check-false (string-ends? "short" "longer")
) ;check-false
(check-true (string-ends? "longer" "er")
) ;check-true
(check-true (string-ends? "a" "a"))
(check-false (string-ends? "a" "ab"))

(check-true (string-ends? "HelloWorld" "World")
) ;check-true
(check-false (string-ends? "HelloWorld" "world")
) ;check-false
(check-true (string-ends? "TestCase" "Case")
) ;check-true
(check-false (string-ends? "TestCase" "case")
) ;check-false

(check-true (string-ends? "document.txt" ".txt")
) ;check-true
(check-true (string-ends? "report.pdf" ".pdf")
) ;check-true
(check-true (string-ends? "config.json" ".json")
) ;check-true
(check-true (string-ends? "image.jpeg" ".jpeg")
) ;check-true
(check-false (string-ends? "document.txt" ".pdf")
) ;check-false
(check-false (string-ends? "noextension" ".txt")
) ;check-false

(check-true (string-ends? "app-v1.0.0" "1.0.0")
) ;check-true
(check-true (string-ends? "release-alpha" "-alpha")
) ;check-true
(check-true (string-ends? "build-SNAPSHOT"
              "SNAPSHOT"
            ) ;string-ends?
) ;check-true
(check-true (string-ends? "product-beta" "-beta")
) ;check-true

(check-true (string-ends? "/api/v1/users" "users")
) ;check-true
(check-true (string-ends? "/index.html" ".html")
) ;check-true
(check-true (string-ends? "/api/endpoint/" "/")
) ;check-true
(check-false (string-ends? "/api/users" "admin")
) ;check-false

(check-true (string-ends? "DatabaseImpl" "Impl")
) ;check-true
(check-true (string-ends? "UserService" "Service")
) ;check-true
(check-true (string-ends? "DataMapper" "Mapper")
) ;check-true
(check-true (string-ends? "FileHandler" "Handler")
) ;check-true
(check-false (string-ends? "SimpleClass" "Utils")
) ;check-false

(check-true (string-ends? "中文测试" "测试")
) ;check-true
(check-true (string-ends? "文件名" "名")
) ;check-true
(check-true (string-ends? "项目说明" "说明")
) ;check-true
(check-true (string-ends? "emoji测试" "测试")
) ;check-true
(check-false (string-ends? "中文文件" "测试")
) ;check-false

(check-true (string-ends? "文件🌟txt" "txt")
) ;check-true
(check-true (string-ends? "配置📄json" "json")
) ;check-true
(check-true (string-ends? "测试✅中文"
              "中文"
            ) ;string-ends?
) ;check-true
(check-true (string-ends? "混合😀表情"
              "表情"
            ) ;string-ends?
) ;check-true

(check-true (string-ends? "Hello😀" "😀")
) ;check-true
(check-true (string-ends? "Star ⭐" "⭐")
) ;check-true
(check-true (string-ends? "表情😂😃" "😃")
) ;check-true
(check-false (string-ends? "Hello😀" "😂")
) ;check-false

(check-true (string-ends? "测试中文编程"
              "编程"
            ) ;string-ends?
) ;check-true
(check-true (string-ends? "Japanese文字日本語"
              "日本語"
            ) ;string-ends?
) ;check-true
(check-true (string-ends? "Korean한국어"
              "한국어"
            ) ;string-ends?
) ;check-true
(check-true (string-ends? "数学方程式equation"
              "equation"
            ) ;string-ends?
) ;check-true

(check-true (string-ends? "config-file-name" "name")
) ;check-true
(check-true (string-ends? "user_name_123" "123")
) ;check-true
(check-true (string-ends? "file-name_ver2.0" "2.0")
) ;check-true
(check-false (string-ends? "config-file" "name")
) ;check-false

(check-true (string-ends? "data.csv" ".csv")
) ;check-true
(check-true (string-ends? "backup.sql" ".sql")
) ;check-true
(check-true (string-ends? "archive.zip" ".zip")
) ;check-true
(check-true (string-ends? "logfile.log" ".log")
) ;check-true
(check-true (string-ends? "script.py" ".py")
) ;check-true

(check-true (string-ends? "1" "1"))
(check-true (string-ends? "12" "2"))
(check-true (string-ends? "123" "3"))
(check-true (string-ends? "1234" "4"))
(check-false (string-ends? "123" "xyz"))

(check-true (string-ends? "a" "a"))
(check-true (string-ends? "ab" "b"))
(check-true (string-ends? "abc" "c"))
(check-false (string-ends? "a" "ab"))
(check-false (string-ends? "ab" "abc"))

(check-true (string-ends? "CustomerData.java"
              ".java"
            ) ;string-ends?
) ;check-true
(check-true (string-ends? "UserRepositoryImpl"
              "Impl"
            ) ;string-ends?
) ;check-true
(check-true (string-ends? "api_response.json"
              ".json"
            ) ;string-ends?
) ;check-true
(check-true (string-ends? "daily_report_2023-08-08.csv"
              ".csv"
            ) ;string-ends?
) ;check-true

(check-true (string-ends? "equation=x+y+z" "z")
) ;check-true
(check-true (string-ends? "math_pi=3.14159" "14159")
) ;check-true
(check-true (string-ends? "temperature_25°C" "°C")
) ;check-true
(check-false (string-ends? "formula=area" "volume")
) ;check-false

(check-true (string-ends? "index.min.js" ".js")
) ;check-true
(check-true (string-ends? "styles.css.map" ".map")
) ;check-true
(check-true (string-ends? "bundle.js.gz" ".gz")
) ;check-true
(check-true (string-ends? "app.d.ts" ".ts")
) ;check-true

(check-true (string-ends? "README.md" ".md")
) ;check-true
(check-true (string-ends? "CHANGELOG.rst" ".rst")
) ;check-true
(check-true (string-ends? "LICENSE.txt" ".txt")
) ;check-true
(check-true (string-ends? "Makefile" "file")
) ;check-true

(check-true (string-ends? "backup_20230808" "08")
) ;check-true
(check-true (string-ends? "log_2023-08-08_15:30:00"
              ":00"
            ) ;string-ends?
) ;check-true
(check-true (string-ends? "event_20230808T153000Z"
              "000Z"
            ) ;string-ends?
) ;check-true

(check-catch 'type-error
  (string-ends? 123 "test")
) ;check-catch
(check-catch 'type-error
  (string-ends? "test" #f)
) ;check-catch
(check-catch 'type-error
  (string-ends? #t "suffix")
) ;check-catch
(check-catch 'type-error
  (string-ends? 'symbol "test")
) ;check-catch
(check-catch 'type-error
  (string-ends? "hello" 456)
) ;check-catch
(check-catch 'type-error
  (string-ends? 'name "test")
) ;check-catch
(check-catch 'type-error
  (string-ends? 123 456)
) ;check-catch
(check-catch 'type-error
  (string-ends? "test" 'invalid)
) ;check-catch
(check-catch 'type-error
  (string-ends? #f #t)
) ;check-catch
(check-catch 'type-error
  (string-ends? '() "test")
) ;check-catch
(check-catch 'type-error
  (string-ends? "hello" '())
) ;check-catch

(check-catch 'type-error
  (string-ends? 0 "suffix")
) ;check-catch
(check-catch 'type-error
  (string-ends? "" 0)
) ;check-catch
(check-catch 'type-error
  (string-ends? 1.5 "test")
) ;check-catch
(check-catch 'type-error
  (string-ends? "string" 2.0)
) ;check-catch

(check-catch 'type-error
  (string-ends? '(1 2 3) "test")
) ;check-catch
(check-catch 'type-error
  (string-ends? "test" '(1 2 3))
) ;check-catch
(check-catch 'type-error
  (string-ends? 999 "test")
) ;check-catch
(check-catch 'type-error
  (string-ends? "valid" 888)
) ;check-catch

(check-report)
