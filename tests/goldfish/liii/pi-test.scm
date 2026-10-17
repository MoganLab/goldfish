(import (liii check)
        (liii string)
        (liii pi))

(check-set-mode! 'report-failed)

;; resolve-app-mode
(check (resolve-app-mode "json" #f #t #t) => 'json)
(check (resolve-app-mode 'json #f #t #t) => 'json)
(check (resolve-app-mode "text" #f #t #t) => 'print)
(check (resolve-app-mode #f #t #t #t) => 'print)
(check (resolve-app-mode #f #f #f #t) => 'print)
(check (resolve-app-mode #f #f #t #f) => 'print)
(check (resolve-app-mode #f #f #t #t) => 'print)

;; format-file-block
(check (format-file-block "/path/to/foo.txt" "hello world")
  => "<file name=\"/path/to/foo.txt\">\nhello world\n</file>\n")
(check (format-file-block "/path/to/bar.txt" "line 1\nline 2")
  => "<file name=\"/path/to/bar.txt\">\nline 1\nline 2\n</file>\n")

;; build-initial-prompt:
;; parts = [stdin?, file-text?, first-message] 无分隔符直接 join
;; 1. 只有 message
(let-values (((prompt rest) (build-initial-prompt #f "" '("message 1" "message 2"))))
  (check prompt => "message 1")
  (check rest => '("message 2")))

;; 2. stdin + message
(let-values (((prompt rest) (build-initial-prompt "stdin content\n" "" '("first message"))))
  (check prompt => "stdin content\nfirst message")
  (check rest => '()))

;; 3. stdin + file + message
(let-values (((prompt rest)
              (build-initial-prompt "stdin content\n"
                                   "<file name=\"/foo\">bar</file>\n"
                                   '("do something" "followup"))))
  (check prompt => "stdin content\n<file name=\"/foo\">bar</file>\ndo something")
  (check rest => '("followup")))

;; 4. 只有 file-text，无 messages
(let-values (((prompt rest)
              (build-initial-prompt #f "<file name=\"/foo\">bar</file>\n" '())))
  (check prompt => "<file name=\"/foo\">bar</file>\n")
  (check rest => '()))

;; 5. 全部为空
(let-values (((prompt rest) (build-initial-prompt #f "" '())))
  (check prompt => "")
  (check rest => '()))

;; 截断常量
(check PI_DEFAULT_MAX_LINES => 2000)
(check PI_DEFAULT_MAX_BYTES => 51200)
(check PI_GREP_MAX_LINE_LENGTH => 500)

;; pi-format-size
(check (pi-format-size 500) => "500B")
(check (pi-format-size 1024) => "1.0KB")
(check (pi-format-size 1536) => "1.5KB")
(check (pi-format-size 51200) => "50.0KB")
(check (pi-format-size (* 1024 1024)) => "1.0MB")

;; pi-truncate-line
(let-values (((line truncated?) (pi-truncate-line "hello world")))
  (check line => "hello world")
  (check truncated? => #f))

(let* ((long-line (make-string 600 #\a)))
  (let-values (((line truncated?) (pi-truncate-line long-line)))
    (check (string-length line) => 500)
    (check truncated? => #t)))

;; pi-truncate-head: 未超出限制
(let ((res (pi-truncate-head "line1\nline2\nline3" 10 1000)))
  (check (cdr (assoc 'truncated res)) => #f)
  (check (cdr (assoc 'content res)) => "line1\nline2\nline3")
  (check (cdr (assoc 'output-lines res)) => 3)
  (check (cdr (assoc 'total-lines res)) => 3))

;; pi-truncate-head: 按行截断
(let ((res (pi-truncate-head "line1\nline2\nline3\nline4" 2 1000)))
  (check (cdr (assoc 'truncated res)) => #t)
  (check (cdr (assoc 'truncated-by res)) => 'lines)
  (check (cdr (assoc 'content res)) => "line1\nline2")
  (check (cdr (assoc 'output-lines res)) => 2)
  (check (cdr (assoc 'total-lines res)) => 4))

;; pi-truncate-head: 按字节截断
(let ((res (pi-truncate-head "12345\n67890\nabcde" 10 12)))
  (check (cdr (assoc 'truncated res)) => #t)
  (check (cdr (assoc 'truncated-by res)) => 'bytes)
  ;; "12345" (5) + \n (1) + "67890" (5) = 11 <= 12; 第三行 +1+5=17 > 12
  (check (cdr (assoc 'content res)) => "12345\n67890")
  (check (cdr (assoc 'output-lines res)) => 2))

;; pi-truncate-head: 第一行单行超字节限
(let ((res (pi-truncate-head "1234567890\nline2" 10 5)))
  (check (cdr (assoc 'truncated res)) => #t)
  (check (cdr (assoc 'truncated-by res)) => 'bytes)
  (check (cdr (assoc 'first-line-exceeds-limit? res)) => #t)
  (check (cdr (assoc 'content res)) => ""))

;; pi-truncate-tail: 未超限
(let ((res (pi-truncate-tail "line1\nline2\nline3" 10 1000)))
  (check (cdr (assoc 'truncated res)) => #f)
  (check (cdr (assoc 'content res)) => "line1\nline2\nline3"))

;; pi-truncate-tail: 按行截断（保留尾部行）
(let ((res (pi-truncate-tail "line1\nline2\nline3\nline4" 2 1000)))
  (check (cdr (assoc 'truncated res)) => #t)
  (check (cdr (assoc 'truncated-by res)) => 'lines)
  (check (cdr (assoc 'content res)) => "line3\nline4")
  (check (cdr (assoc 'output-lines res)) => 2))

;; pi-truncate-tail: 按字节截断（保留尾部）
(let ((res (pi-truncate-tail "12345\n67890\nabcde" 10 12)))
  (check (cdr (assoc 'truncated res)) => #t)
  (check (cdr (assoc 'truncated-by res)) => 'bytes)
  (check (cdr (assoc 'content res)) => "67890\nabcde"))

;; ============================================================
;; Tool: read
;; ============================================================

;; 1. 读取存在的文件
(let ((res (pi-execute-read '(("path" . "tests/goldfish/liii/pi-test.scm")))))
  (check-true (string? res))
  (check-true (string-contains? res "(check-set-mode! 'report-failed)")))

;; 2. 带 limit 读取（包含剩余行提示）
(let ((res (pi-execute-read '(("path" . "tests/goldfish/liii/pi-test.scm") ("limit" . 2)))))
  (check-true (string-contains? res "more lines in file. Use offset=3 to continue.")))

;; 3. 带 offset 读取
(let ((res (pi-execute-read '(("path" . "tests/goldfish/liii/pi-test.scm") ("offset" . 3) ("limit" . 2)))))
  (check-true (string-contains? res "more lines in file. Use offset=5 to continue.")))

;; 4. offset 超出文件总行数
(check-catch 'error
  (pi-execute-read '(("path" . "tests/goldfish/liii/pi-test.scm") ("offset" . 999999))))

;; 5. 文件不存在报错
(check-catch 'error
  (pi-execute-read '(("path" . "non_existent_file_12345.txt"))))

;; 6. 路径为目录报错
(check-catch 'error
  (pi-execute-read '(("path" . "tests"))))

;; 7. 超时路径测试
(check-catch 'error
  (pi-execute-read '(("path" . "tests/goldfish/liii/pi-test.scm")) :timed-out? #t))

(check-report)
