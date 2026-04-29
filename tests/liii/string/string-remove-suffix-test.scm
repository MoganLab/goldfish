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

(check (string-remove-suffix "aaa" "a") => "aa")
(check (string-remove-suffix "aaa" "") => "aaa")
(check (string-remove-suffix "Goldfish.tmu" ".tmu") => "Goldfish")

(check (string-remove-suffix "filename.txt" ".txt") => "filename")
(check (string-remove-suffix "test.js" ".js") => "test")
(check (string-remove-suffix "document.pdf" ".pdf") => "document")
(check (string-remove-suffix "hello world" "world") => "hello ")
(check (string-remove-suffix "scheme.scm" ".scm") => "scheme")

(check (string-remove-suffix "hello.txt" ".js") => "hello.txt")
(check (string-remove-suffix "abcdef" "xyz") => "abcdef")
(check (string-remove-suffix "test" "longsuffix") => "test")

(check (string-remove-suffix "" "") => "")
(check (string-remove-suffix "test" "") => "test")
(check (string-remove-suffix "" "test") => "")

(check (string-remove-suffix "a" "a") => "")
(check (string-remove-suffix "a" "b") => "a")
(check (string-remove-suffix "abc" "c") => "ab")

(check (string-remove-suffix "hello" "hello") => "")
(check (string-remove-suffix "test" "test") => "")

(check (string-remove-suffix "file.tar.gz" ".gz") => "file.tar")
(check (string-remove-suffix "file.tar.gz" ".tar.gz") => "file")

(check (string-remove-suffix "中文文档.txt" ".txt") => "中文文档")
(check (string-remove-suffix "测试文件.json" ".json") => "测试文件")
(check (string-remove-suffix "金鱼缸.tmu" ".tmu") => "金鱼缸")
(check (string-remove-suffix "文件" "文件") => "")

(check (string-remove-suffix "/path/to/file.txt" ".txt") => "/path/to/file")
(check (string-remove-suffix "C:\\Windows\\test.exe" ".exe")
  =>
  "C:\\Windows\\test"
) ;check

(check (string-remove-suffix "aaaa" "aa") => "aa")
(check (string-remove-suffix "aaa" "aa") => "a")
(check (string-remove-suffix "aaaa" "aaa") => "a")

(check (string-remove-suffix "application.log.backup" ".backup")
  =>
  "application.log"
) ;check
(check (string-remove-suffix "data.2024.01.15.csv" ".csv") => "data.2024.01.15")

(check (string-remove-suffix "test-file_name.backup.suffix" ".suffix")
  =>
  "test-file_name.backup"
) ;check
(check (string-remove-suffix "user@domain.com" "@domain.com") => "user")
(check (string-remove-suffix "http://example.com" ".com") => "http://example")

(check (string-remove-suffix "temp123.tmp" ".tmp") => "temp123")
(check (string-remove-suffix "file2024.log" ".log") => "file2024")

(check (string-remove-suffix "image.png.backup" ".backup") => "image.png")
(check (string-remove-suffix "document.pdf.encrypted" ".encrypted")
  =>
  "document.pdf"
) ;check

(check (string-remove-suffix "TEST.TXT" ".txt") => "TEST.TXT")
(check (string-remove-suffix "Test.TXT" ".TXT") => "Test")
(check (string-remove-suffix "hello.TXT" ".txt") => "hello.TXT")

(check (string-remove-suffix "filename.tar.gz" ".gz") => "filename.tar")
(check (string-remove-suffix "/var/log/app.log" ".log") => "/var/log/app")
(check (string-remove-suffix "./config.json" ".json") => "./config")

(check-catch 'type-error (string-remove-suffix 123 "test"))
(check-catch 'type-error (string-remove-suffix "test" 123))
(check-catch 'type-error (string-remove-suffix 'symbol "test"))
(check-catch 'type-error (string-remove-suffix "test" '(not-a-string)))
(check-catch 'type-error (string-remove-suffix 123.5 "suffix"))
(check-catch 'type-error (string-remove-suffix "filename" 123.45))
(check-catch 'type-error (string-remove-suffix '(1 2 3) ".txt"))
(check-catch 'type-error (string-remove-suffix "text" #\c))

(check (string-remove-suffix "中文测试文件.txt" ".txt")
  =>
  "中文测试文件"
) ;check
(check (string-remove-suffix "中文.json" ".json") => "中文")
(check (string-remove-suffix "引用的文件.js" ".js") => "引用的文件")

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
      (modified (string-remove-suffix "application.log" ".log"))
     ) ;
  (check-true (equal? modified "application"))
  (check-false (eq? original modified))
) ;let

(check-report)
