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

(check (string-remove-prefix "浙江省杭州市西湖区" "浙江省")
  =>
  "杭州市西湖区"
) ;check
(check (string-remove-prefix "aaa" "a") => "aa")
(check (string-remove-prefix "abc" "bc") => "abc")
(check (string-remove-prefix "abc" "") => "abc")

(check (string-remove-prefix "filename.txt" "file") => "name.txt")
(check (string-remove-prefix "database.sql" "data") => "base.sql")
(check (string-remove-prefix "test.js" "test") => ".js")
(check (string-remove-prefix "hello world" "hello") => " world")
(check (string-remove-prefix "scheme.scm" "scheme") => ".scm")

(check (string-remove-prefix "hello.txt" "world") => "hello.txt")
(check (string-remove-prefix "abcdef" "xyz") => "abcdef")
(check (string-remove-prefix "test" "longprefix") => "test")

(check (string-remove-prefix "" "") => "")
(check (string-remove-prefix "test" "") => "test")
(check (string-remove-prefix "" "test") => "")

(check (string-remove-prefix "a" "a") => "")
(check (string-remove-prefix "a" "b") => "a")
(check (string-remove-prefix "abc" "a") => "bc")
(check (string-remove-prefix "abc" "b") => "abc")

(check (string-remove-prefix "hello" "hello") => "")
(check (string-remove-prefix "test" "test") => "")

(check (string-remove-prefix "path/to/file" "path/") => "to/file")
(check (string-remove-prefix "very.long.filename" "very.") => "long.filename")

(check (string-remove-prefix "中文文档.txt" "中文") => "文档.txt")
(check (string-remove-prefix "测试文件.json" "测试") => "文件.json")
(check (string-remove-prefix "金鱼缸.tmu" "金鱼缸.") => "tmu")
(check (string-remove-prefix "浙江省" "浙江") => "省")

(check (string-remove-prefix "/usr/local/app" "/usr/local") => "/app")
(check (string-remove-prefix "C:\\Windows\\app.exe" "C:\\Windows\\")
  =>
  "app.exe"
) ;check
(check (string-remove-prefix "/home/user/data" "/home/") => "user/data")

(check (string-remove-prefix "aaaa" "aa") => "aa")
(check (string-remove-prefix "aaa" "aa") => "a")
(check (string-remove-prefix "aaaa" "aaa") => "a")

(check (string-remove-prefix "www.example.com" "www.") => "example.com")
(check (string-remove-prefix "https://website.com" "https://") => "website.com")
(check (string-remove-prefix "admin@domain.com" "admin@") => "domain.com")

(check (string-remove-prefix "process_file.txt" "process_") => "file.txt")
(check (string-remove-prefix "backup_data_2024.json" "backup_")
  =>
  "data_2024.json"
) ;check
(check (string-remove-prefix "temp_folder_backup" "temp_") => "folder_backup")

(check (string-remove-prefix "log2024.txt" "log") => "2024.txt")
(check (string-remove-prefix "test123.json" "test") => "123.json")
(check (string-remove-prefix "user2024" "user") => "2024")

(check (string-remove-prefix "converted_data_processed.json" "converted_")
  =>
  "data_processed.json"
) ;check
(check (string-remove-prefix "converted_data_processed.json" "converted_data_")
  =>
  "processed.json"
) ;check

(check (string-remove-prefix "test-file_name.src" "test-") => "file_name.src")
(check (string-remove-prefix "user@domain.com" "user@") => "domain.com")
(check (string-remove-prefix "user_name_data" "user_name_") => "data")

(check (string-remove-prefix "data:12345" "data:") => "12345")
(check (string-remove-prefix "json:{\"key\":\"value\"}" "json:")
  =>
  "{\"key\":\"value\"}"
) ;check

(check (string-remove-prefix "v2.0.config" "v2.0.") => "config")
(check (string-remove-prefix "v1.2.3.release" "v1.") => "2.3.release")

(check (string-remove-prefix "TEST.TXT" "test") => "TEST.TXT")
(check (string-remove-prefix "Test.TXT" "Test") => ".TXT")
(check (string-remove-prefix "HELLO" "hello") => "HELLO")

(check (string-remove-prefix "/var/log/httpd/access.log" "/var/log/httpd/")
  =>
  "access.log"
) ;check
(check (string-remove-prefix "./config/production.yml" "./config/")
  =>
  "production.yml"
) ;check
(check (string-remove-prefix "backup/config/app.js" "backup/")
  =>
  "config/app.js"
) ;check

(check (string-remove-prefix "functionName(param)" "functionName(") => "param)")
(check (string-remove-prefix "main(int argc)" "main(") => "int argc)")

(check (string-remove-prefix "MyClass.method" "MyClass.") => "method")
(check (string-remove-prefix "module.submodule" "module.") => "submodule")

(check (string-remove-prefix "2024-08-08.log" "2024-08-08.") => "log")
(check (string-remove-prefix "20240808_143022_backup" "20240808_")
  =>
  "143022_backup"
) ;check

(check (string-remove-prefix "中文测试文件.json" "中文测试")
  =>
  "文件.json"
) ;check
(check (string-remove-prefix "中文测试文件.json" "中文")
  =>
  "测试文件.json"
) ;check
(check (string-remove-prefix "引用的文件.js" "引用的") => "文件.js")

(check (string-remove-prefix "very-long-prefix-file.txt" "very")
  =>
  "-long-prefix-file.txt"
) ;check
(check (string-remove-prefix "short" "very-long-prefix") => "short")

(check (string-remove-prefix "ID_12345_info" "ID_") => "12345_info")
(check (string-remove-prefix "DB_table_name" "DB_") => "table_name")

(check (string-remove-prefix "final_data.py" "final_") => "data.py")
(check (string-remove-prefix "static_function.js" "static_") => "function.js")

(check (string-remove-prefix "  file.txt" "  ") => "file.txt")
(check (string-remove-prefix "\tconfig.yml" "\t") => "config.yml")
(check (string-remove-prefix "\nscript.sh" "\n") => "script.sh")

(check-catch 'type-error (string-remove-prefix 123 "test"))
(check-catch 'type-error (string-remove-prefix "test" 123))
(check-catch 'type-error (string-remove-prefix 'symbol "test"))
(check-catch 'type-error (string-remove-prefix "test" '(not-a-string)))
(check-catch 'type-error (string-remove-prefix 123.5 "prefix"))
(check-catch 'type-error (string-remove-prefix "filename" 123.45))
(check-catch 'type-error (string-remove-prefix '(1 2 3) "prefix"))
(check-catch 'type-error (string-remove-prefix "text" #\c))

(check (let ((filename "my-namespace.module"))
         (string-remove-prefix filename "my-namespace.")
       ) ;let
  =>
  "module"
) ;check

(check (let ((path "/usr/local/lib/module.py"))
         (string-remove-prefix path "/usr/local/lib/")
       ) ;let
  =>
  "module.py"
) ;check

(let ((original "application.js")
      (modified (string-remove-prefix "application.js" "application"))
     ) ;
  (check-true (equal? modified ".js"))
  (check-false (eq? original modified))
) ;let

(check-report)
