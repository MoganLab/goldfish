(import (liii check) (liii string))

;; string-remove-prefix
;; 如果字符串以指定前缀开始，则移除该前缀；否则返回原字符串。
;;
;; 语法
;; ----
;; (string-remove-prefix str prefix)
;;
;; 参数
;; ----
;; str : string?
;; 要处理的源字符串。
;;
;; prefix : string?
;; 要移除的前缀字符串。
;;
;; 返回值
;; ----
;; string
;; - 如果str以prefix开头，返回移除prefix后的新字符串。
;; - 如果str不以prefix开头，返回原字符串的副本。
;; - 如果prefix为空字符串，返回原字符串的副本。
;;
;; 注意
;; ----
;; string-remove-prefix使用string-prefix?来判断字符串是否以prefix开始。
;; 移除前缀是指将字符串开头与prefix匹配的部分删除，返回剩余部分。
;; 该函数返回新的字符串对象，而不是修改原字符串。
;;
;; 错误处理
;; ----
;; type-error 当参数不是字符串类型时。需要两个参数都是字符串。

(check (string-remove-prefix "浙江省杭州市西湖区"
         "浙江省"
       ) ;string-remove-prefix
  =>
  "杭州市西湖区"
) ;check
(check (string-remove-prefix "aaa" "a")
  =>
  "aa"
) ;check
(check (string-remove-prefix "abc" "bc")
  =>
  "abc"
) ;check
(check (string-remove-prefix "abc" "")
  =>
  "abc"
) ;check

(check (string-remove-prefix "filename.txt"
         "file"
       ) ;string-remove-prefix
  =>
  "name.txt"
) ;check
(check (string-remove-prefix "database.sql"
         "data"
       ) ;string-remove-prefix
  =>
  "base.sql"
) ;check
(check (string-remove-prefix "test.js" "test")
  =>
  ".js"
) ;check
(check (string-remove-prefix "hello world"
         "hello"
       ) ;string-remove-prefix
  =>
  " world"
) ;check
(check (string-remove-prefix "scheme.scm"
         "scheme"
       ) ;string-remove-prefix
  =>
  ".scm"
) ;check

(check (string-remove-prefix "hello.txt"
         "world"
       ) ;string-remove-prefix
  =>
  "hello.txt"
) ;check
(check (string-remove-prefix "abcdef" "xyz")
  =>
  "abcdef"
) ;check
(check (string-remove-prefix "test"
         "longprefix"
       ) ;string-remove-prefix
  =>
  "test"
) ;check

(check (string-remove-prefix "" "")
  =>
  ""
) ;check
(check (string-remove-prefix "test" "")
  =>
  "test"
) ;check
(check (string-remove-prefix "" "test")
  =>
  ""
) ;check

(check (string-remove-prefix "a" "a")
  =>
  ""
) ;check
(check (string-remove-prefix "a" "b")
  =>
  "a"
) ;check
(check (string-remove-prefix "abc" "a")
  =>
  "bc"
) ;check
(check (string-remove-prefix "abc" "b")
  =>
  "abc"
) ;check

(check (string-remove-prefix "hello" "hello")
  =>
  ""
) ;check
(check (string-remove-prefix "test" "test")
  =>
  ""
) ;check

(check (string-remove-prefix "path/to/file"
         "path/"
       ) ;string-remove-prefix
  =>
  "to/file"
) ;check
(check (string-remove-prefix "very.long.filename"
         "very."
       ) ;string-remove-prefix
  =>
  "long.filename"
) ;check

(check (string-remove-prefix "中文文档.txt"
         "中文"
       ) ;string-remove-prefix
  =>
  "文档.txt"
) ;check
(check (string-remove-prefix "测试文件.json"
         "测试"
       ) ;string-remove-prefix
  =>
  "文件.json"
) ;check
(check (string-remove-prefix "金鱼缸.tmu"
         "金鱼缸."
       ) ;string-remove-prefix
  =>
  "tmu"
) ;check
(check (string-remove-prefix "浙江省"
         "浙江"
       ) ;string-remove-prefix
  =>
  "省"
) ;check

(check (string-remove-prefix "/usr/local/app"
         "/usr/local"
       ) ;string-remove-prefix
  =>
  "/app"
) ;check
(check (string-remove-prefix "C:\\Windows\\app.exe"
         "C:\\Windows\\"
       ) ;string-remove-prefix
  =>
  "app.exe"
) ;check
(check (string-remove-prefix "/home/user/data"
         "/home/"
       ) ;string-remove-prefix
  =>
  "user/data"
) ;check

(check (string-remove-prefix "aaaa" "aa")
  =>
  "aa"
) ;check
(check (string-remove-prefix "aaa" "aa")
  =>
  "a"
) ;check
(check (string-remove-prefix "aaaa" "aaa")
  =>
  "a"
) ;check

(check (string-remove-prefix "www.example.com"
         "www."
       ) ;string-remove-prefix
  =>
  "example.com"
) ;check
(check (string-remove-prefix "https://website.com"
         "https://"
       ) ;string-remove-prefix
  =>
  "website.com"
) ;check
(check (string-remove-prefix "admin@domain.com"
         "admin@"
       ) ;string-remove-prefix
  =>
  "domain.com"
) ;check

(check (string-remove-prefix "process_file.txt"
         "process_"
       ) ;string-remove-prefix
  =>
  "file.txt"
) ;check
(check (string-remove-prefix "backup_data_2024.json"
         "backup_"
       ) ;string-remove-prefix
  =>
  "data_2024.json"
) ;check
(check (string-remove-prefix "temp_folder_backup"
         "temp_"
       ) ;string-remove-prefix
  =>
  "folder_backup"
) ;check

(check (string-remove-prefix "log2024.txt"
         "log"
       ) ;string-remove-prefix
  =>
  "2024.txt"
) ;check
(check (string-remove-prefix "test123.json"
         "test"
       ) ;string-remove-prefix
  =>
  "123.json"
) ;check
(check (string-remove-prefix "user2024" "user")
  =>
  "2024"
) ;check

(check (string-remove-prefix "converted_data_processed.json"
         "converted_"
       ) ;string-remove-prefix
  =>
  "data_processed.json"
) ;check
(check (string-remove-prefix "converted_data_processed.json"
         "converted_data_"
       ) ;string-remove-prefix
  =>
  "processed.json"
) ;check

(check (string-remove-prefix "test-file_name.src"
         "test-"
       ) ;string-remove-prefix
  =>
  "file_name.src"
) ;check
(check (string-remove-prefix "user@domain.com"
         "user@"
       ) ;string-remove-prefix
  =>
  "domain.com"
) ;check
(check (string-remove-prefix "user_name_data"
         "user_name_"
       ) ;string-remove-prefix
  =>
  "data"
) ;check

(check (string-remove-prefix "data:12345"
         "data:"
       ) ;string-remove-prefix
  =>
  "12345"
) ;check
(check (string-remove-prefix "json:{\"key\":\"value\"}"
         "json:"
       ) ;string-remove-prefix
  =>
  "{\"key\":\"value\"}"
) ;check

(check (string-remove-prefix "v2.0.config"
         "v2.0."
       ) ;string-remove-prefix
  =>
  "config"
) ;check
(check (string-remove-prefix "v1.2.3.release"
         "v1."
       ) ;string-remove-prefix
  =>
  "2.3.release"
) ;check

(check (string-remove-prefix "TEST.TXT" "test")
  =>
  "TEST.TXT"
) ;check
(check (string-remove-prefix "Test.TXT" "Test")
  =>
  ".TXT"
) ;check
(check (string-remove-prefix "HELLO" "hello")
  =>
  "HELLO"
) ;check

(check (string-remove-prefix "/var/log/httpd/access.log"
         "/var/log/httpd/"
       ) ;string-remove-prefix
  =>
  "access.log"
) ;check
(check (string-remove-prefix "./config/production.yml"
         "./config/"
       ) ;string-remove-prefix
  =>
  "production.yml"
) ;check
(check (string-remove-prefix "backup/config/app.js"
         "backup/"
       ) ;string-remove-prefix
  =>
  "config/app.js"
) ;check

(check (string-remove-prefix "functionName(param)"
         "functionName("
       ) ;string-remove-prefix
  =>
  "param)"
) ;check
(check (string-remove-prefix "main(int argc)"
         "main("
       ) ;string-remove-prefix
  =>
  "int argc)"
) ;check

(check (string-remove-prefix "MyClass.method"
         "MyClass."
       ) ;string-remove-prefix
  =>
  "method"
) ;check
(check (string-remove-prefix "module.submodule"
         "module."
       ) ;string-remove-prefix
  =>
  "submodule"
) ;check

(check (string-remove-prefix "2024-08-08.log"
         "2024-08-08."
       ) ;string-remove-prefix
  =>
  "log"
) ;check
(check (string-remove-prefix "20240808_143022_backup"
         "20240808_"
       ) ;string-remove-prefix
  =>
  "143022_backup"
) ;check

(check (string-remove-prefix "中文测试文件.json"
         "中文测试"
       ) ;string-remove-prefix
  =>
  "文件.json"
) ;check
(check (string-remove-prefix "中文测试文件.json"
         "中文"
       ) ;string-remove-prefix
  =>
  "测试文件.json"
) ;check
(check (string-remove-prefix "引用的文件.js"
         "引用的"
       ) ;string-remove-prefix
  =>
  "文件.js"
) ;check

(check (string-remove-prefix "very-long-prefix-file.txt"
         "very"
       ) ;string-remove-prefix
  =>
  "-long-prefix-file.txt"
) ;check
(check (string-remove-prefix "short"
         "very-long-prefix"
       ) ;string-remove-prefix
  =>
  "short"
) ;check

(check (string-remove-prefix "ID_12345_info"
         "ID_"
       ) ;string-remove-prefix
  =>
  "12345_info"
) ;check
(check (string-remove-prefix "DB_table_name"
         "DB_"
       ) ;string-remove-prefix
  =>
  "table_name"
) ;check

(check (string-remove-prefix "final_data.py"
         "final_"
       ) ;string-remove-prefix
  =>
  "data.py"
) ;check
(check (string-remove-prefix "static_function.js"
         "static_"
       ) ;string-remove-prefix
  =>
  "function.js"
) ;check

(check (string-remove-prefix "  file.txt" "  ")
  =>
  "file.txt"
) ;check
(check (string-remove-prefix "\tconfig.yml"
         "\t"
       ) ;string-remove-prefix
  =>
  "config.yml"
) ;check
(check (string-remove-prefix "\nscript.sh"
         "\n"
       ) ;string-remove-prefix
  =>
  "script.sh"
) ;check

(check-catch 'type-error
  (string-remove-prefix 123 "test")
) ;check-catch
(check-catch 'type-error
  (string-remove-prefix "test" 123)
) ;check-catch
(check-catch 'type-error
  (string-remove-prefix 'symbol "test")
) ;check-catch
(check-catch 'type-error
  (string-remove-prefix "test"
    '(not-a-string)
  ) ;string-remove-prefix
) ;check-catch
(check-catch 'type-error
  (string-remove-prefix 123.5 "prefix")
) ;check-catch
(check-catch 'type-error
  (string-remove-prefix "filename" 123.45)
) ;check-catch
(check-catch 'type-error
  (string-remove-prefix '(1 2 3) "prefix")
) ;check-catch
(check-catch 'type-error
  (string-remove-prefix "text" #\c)
) ;check-catch

(check (let ((filename "my-namespace.module"))
         (string-remove-prefix filename
           "my-namespace."
         ) ;string-remove-prefix
       ) ;let
  =>
  "module"
) ;check

(check (let ((path "/usr/local/lib/module.py"))
         (string-remove-prefix path
           "/usr/local/lib/"
         ) ;string-remove-prefix
       ) ;let
  =>
  "module.py"
) ;check

(let ((original "application.js")
      (modified (string-remove-prefix "application.js"
                  "application"
                ) ;string-remove-prefix
      ) ;modified
     ) ;
  (check-true (equal? modified ".js"))
  (check-false (eq? original modified))
) ;let

(check-report)
