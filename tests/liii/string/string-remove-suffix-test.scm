(import (liii check) (liii string))

;; string-remove-suffix
;; 如果字符串以指定后缀结束，则移除该后缀；否则返回原字符串。
;;
;; 语法
;; ----
;; (string-remove-suffix str suffix)
;;
;; 参数
;; ----
;; str : string?
;; 要处理的源字符串。
;;
;; suffix : string?
;; 要移除的后缀字符串。
;;
;; 返回值
;; ----
;; string
;; - 如果str以suffix结尾，返回移除suffix后的新字符串。
;; - 如果str不以suffix结尾，返回原字符串的副本。
;; - 如果suffix为空字符串，返回原字符串的副本。
;;
;; 注意
;; ----
;; string-remove-suffix使用string-suffix?来判断字符串是否以后缀结束。
;; 移除后缀是指将字符串末尾与后缀匹配的部分删除，返回剩余部分。
;; 该函数会返回新的字符串对象，而不是修改原字符串。
;;
;; 示例
;; ----
;; (string-remove-suffix "filename.txt" ".txt") => "filename"
;; (string-remove-suffix "test.js" ".py") => "test.js"
;; (string-remove-suffix "hello world" "world") => "hello "
;; (string-remove-suffix "test" "") => "test"
;; (string-remove-suffix "" "test") => ""
;;
;; 错误处理
;; ----
;; type-error 当参数不是字符串类型时。需要两个参数都是字符串。

(check (string-remove-suffix "aaa" "a")
  =>
  "aa"
) ;check
(check (string-remove-suffix "aaa" "")
  =>
  "aaa"
) ;check
(check (string-remove-suffix "Goldfish.tmu"
         ".tmu"
       ) ;string-remove-suffix
  =>
  "Goldfish"
) ;check

(check (string-remove-suffix "filename.txt"
         ".txt"
       ) ;string-remove-suffix
  =>
  "filename"
) ;check
(check (string-remove-suffix "test.js" ".js")
  =>
  "test"
) ;check
(check (string-remove-suffix "document.pdf"
         ".pdf"
       ) ;string-remove-suffix
  =>
  "document"
) ;check
(check (string-remove-suffix "hello world"
         "world"
       ) ;string-remove-suffix
  =>
  "hello "
) ;check
(check (string-remove-suffix "scheme.scm"
         ".scm"
       ) ;string-remove-suffix
  =>
  "scheme"
) ;check

(check (string-remove-suffix "hello.txt" ".js")
  =>
  "hello.txt"
) ;check
(check (string-remove-suffix "abcdef" "xyz")
  =>
  "abcdef"
) ;check
(check (string-remove-suffix "test"
         "longsuffix"
       ) ;string-remove-suffix
  =>
  "test"
) ;check

(check (string-remove-suffix "" "")
  =>
  ""
) ;check
(check (string-remove-suffix "test" "")
  =>
  "test"
) ;check
(check (string-remove-suffix "" "test")
  =>
  ""
) ;check

(check (string-remove-suffix "a" "a")
  =>
  ""
) ;check
(check (string-remove-suffix "a" "b")
  =>
  "a"
) ;check
(check (string-remove-suffix "abc" "c")
  =>
  "ab"
) ;check

(check (string-remove-suffix "hello" "hello")
  =>
  ""
) ;check
(check (string-remove-suffix "test" "test")
  =>
  ""
) ;check

(check (string-remove-suffix "file.tar.gz"
         ".gz"
       ) ;string-remove-suffix
  =>
  "file.tar"
) ;check
(check (string-remove-suffix "file.tar.gz"
         ".tar.gz"
       ) ;string-remove-suffix
  =>
  "file"
) ;check

(check (string-remove-suffix "中文文档.txt"
         ".txt"
       ) ;string-remove-suffix
  =>
  "中文文档"
) ;check
(check (string-remove-suffix "测试文件.json"
         ".json"
       ) ;string-remove-suffix
  =>
  "测试文件"
) ;check
(check (string-remove-suffix "金鱼缸.tmu"
         ".tmu"
       ) ;string-remove-suffix
  =>
  "金鱼缸"
) ;check
(check (string-remove-suffix "文件" "文件")
  =>
  ""
) ;check

(check (string-remove-suffix "/path/to/file.txt"
         ".txt"
       ) ;string-remove-suffix
  =>
  "/path/to/file"
) ;check
(check (string-remove-suffix "C:\\Windows\\test.exe"
         ".exe"
       ) ;string-remove-suffix
  =>
  "C:\\Windows\\test"
) ;check

(check (string-remove-suffix "aaaa" "aa")
  =>
  "aa"
) ;check
(check (string-remove-suffix "aaa" "aa")
  =>
  "a"
) ;check
(check (string-remove-suffix "aaaa" "aaa")
  =>
  "a"
) ;check

(check (string-remove-suffix "application.log.backup"
         ".backup"
       ) ;string-remove-suffix
  =>
  "application.log"
) ;check
(check (string-remove-suffix "data.2024.01.15.csv"
         ".csv"
       ) ;string-remove-suffix
  =>
  "data.2024.01.15"
) ;check

(check (string-remove-suffix "test-file_name.backup.suffix"
         ".suffix"
       ) ;string-remove-suffix
  =>
  "test-file_name.backup"
) ;check
(check (string-remove-suffix "user@domain.com"
         "@domain.com"
       ) ;string-remove-suffix
  =>
  "user"
) ;check
(check (string-remove-suffix "http://example.com"
         ".com"
       ) ;string-remove-suffix
  =>
  "http://example"
) ;check

(check (string-remove-suffix "temp123.tmp"
         ".tmp"
       ) ;string-remove-suffix
  =>
  "temp123"
) ;check
(check (string-remove-suffix "file2024.log"
         ".log"
       ) ;string-remove-suffix
  =>
  "file2024"
) ;check

(check (string-remove-suffix "image.png.backup"
         ".backup"
       ) ;string-remove-suffix
  =>
  "image.png"
) ;check
(check (string-remove-suffix "document.pdf.encrypted"
         ".encrypted"
       ) ;string-remove-suffix
  =>
  "document.pdf"
) ;check

(check (string-remove-suffix "TEST.TXT" ".txt")
  =>
  "TEST.TXT"
) ;check
(check (string-remove-suffix "Test.TXT" ".TXT")
  =>
  "Test"
) ;check
(check (string-remove-suffix "hello.TXT"
         ".txt"
       ) ;string-remove-suffix
  =>
  "hello.TXT"
) ;check

(check (string-remove-suffix "filename.tar.gz"
         ".gz"
       ) ;string-remove-suffix
  =>
  "filename.tar"
) ;check
(check (string-remove-suffix "/var/log/app.log"
         ".log"
       ) ;string-remove-suffix
  =>
  "/var/log/app"
) ;check
(check (string-remove-suffix "./config.json"
         ".json"
       ) ;string-remove-suffix
  =>
  "./config"
) ;check

(check-catch 'type-error
  (string-remove-suffix 123 "test")
) ;check-catch
(check-catch 'type-error
  (string-remove-suffix "test" 123)
) ;check-catch
(check-catch 'type-error
  (string-remove-suffix 'symbol "test")
) ;check-catch
(check-catch 'type-error
  (string-remove-suffix "test"
    '(not-a-string)
  ) ;string-remove-suffix
) ;check-catch
(check-catch 'type-error
  (string-remove-suffix 123.5 "suffix")
) ;check-catch
(check-catch 'type-error
  (string-remove-suffix "filename" 123.45)
) ;check-catch
(check-catch 'type-error
  (string-remove-suffix '(1 2 3) ".txt")
) ;check-catch
(check-catch 'type-error
  (string-remove-suffix "text" #\c)
) ;check-catch

(check (string-remove-suffix "中文测试文件.txt"
         ".txt"
       ) ;string-remove-suffix
  =>
  "中文测试文件"
) ;check
(check (string-remove-suffix "中文.json"
         ".json"
       ) ;string-remove-suffix
  =>
  "中文"
) ;check
(check (string-remove-suffix "引用的文件.js"
         ".js"
       ) ;string-remove-suffix
  =>
  "引用的文件"
) ;check

(check (let ((filename "program.c"))
         (string-remove-suffix filename ".c")
       ) ;let
  =>
  "program"
) ;check

(check (let ((path "/usr/local/bin/script.py"))
         (string-remove-suffix path ".py")
       ) ;let
  =>
  "/usr/local/bin/script"
) ;check

(let ((original "application.log")
      (modified (string-remove-suffix "application.log"
                  ".log"
                ) ;string-remove-suffix
      ) ;modified
     ) ;
  (check-true (equal? modified "application")
  ) ;check-true
  (check-false (eq? original modified))
) ;let

(check-report)
