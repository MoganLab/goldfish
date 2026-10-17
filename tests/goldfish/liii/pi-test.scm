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

;; ============================================================
;; Tool: write
;; ============================================================

(let ((test-write-file "tests/goldfish/liii/pi-write-fixture/sub/test.txt"))
  ;; 1. 成功写入并自动创建父目录
  (let ((msg (pi-execute-write (list (cons "path" test-write-file)
                                     (cons "content" "hello write tool\nline 2")))))
    (check msg => (string-append "Successfully wrote to " test-write-file))
    ;; 读取验证内容
    (let ((content (pi-execute-read (list (cons "path" test-write-file)))))
      (check content => "hello write tool\nline 2")))

  ;; 2. 覆盖写入
  (let ((msg (pi-execute-write (list (cons "path" test-write-file)
                                     (cons "content" "overwritten content")))))
    (check msg => (string-append "Successfully wrote to " test-write-file))
    (let ((content (pi-execute-read (list (cons "path" test-write-file)))))
      (check content => "overwritten content")))

  ;; 3. 清理测试产生的文件和目录
  (delete-file test-write-file)
  (delete-file "tests/goldfish/liii/pi-write-fixture/sub")
  (delete-file "tests/goldfish/liii/pi-write-fixture"))

;; 4. 缺少参数报错
(check-catch 'error
  (pi-execute-write '(("path" . "foo.txt"))))
(check-catch 'error
  (pi-execute-write '(("content" . "hello"))))

;; 5. 超时路径测试
(check-catch 'error
  (pi-execute-write '(("path" . "foo.txt") ("content" . "bar")) :timed-out? #t))

;; ============================================================
;; Tool: edit
;; ============================================================

(let ((test-edit-file "tests/goldfish/liii/pi-edit-fixture.txt"))
  ;; 准备初始文件
  (pi-execute-write (list (cons "path" test-edit-file)
                          (cons "content" "line 1\nline 2: AAA\nline 3\nline 4: BBB\nline 5")))

  ;; 1. 单个替换成功
  (let ((msg (pi-execute-edit (list (cons "path" test-edit-file)
                                    (cons "edits" (list (list (cons "oldText" "line 2: AAA")
                                                              (cons "newText" "line 2: MODIFIED_A"))))))))
    (check msg => (string-append "Successfully replaced 1 block(s) in " test-edit-file "."))
    (let ((c (pi-execute-read (list (cons "path" test-edit-file)))))
      (check-true (string-contains? c "line 2: MODIFIED_A"))))

  ;; 2. 多个不重叠替换同时生效（倒序应用）
  (let ((msg (pi-execute-edit (list (cons "path" test-edit-file)
                                    (cons "edits" (list (list (cons "oldText" "line 1")
                                                              (cons "newText" "line 1: FIRST"))
                                                        (list (cons "oldText" "line 4: BBB")
                                                              (cons "newText" "line 4: SECOND"))))))))
    (check msg => (string-append "Successfully replaced 2 block(s) in " test-edit-file "."))
    (let ((c (pi-execute-read (list (cons "path" test-edit-file)))))
      (check-true (string-contains? c "line 1: FIRST"))
      (check-true (string-contains? c "line 4: SECOND"))))

  ;; 3. oldText 找不到报错
  (check-catch 'error
    (pi-execute-edit (list (cons "path" test-edit-file)
                           (cons "edits" (list (list (cons "oldText" "NON_EXISTENT")
                                                     (cons "newText" "NEW")))))))

  ;; 4. oldText 不唯一报错
  (pi-execute-write (list (cons "path" test-edit-file)
                          (cons "content" "dup\ndup\n")))
  (check-catch 'error
    (pi-execute-edit (list (cons "path" test-edit-file)
                           (cons "edits" (list (list (cons "oldText" "dup")
                                                     (cons "newText" "new")))))))

  ;; 5. 重叠 edits 报错
  (pi-execute-write (list (cons "path" test-edit-file)
                          (cons "content" "ABCDEFGH")))
  (check-catch 'error
    (pi-execute-edit (list (cons "path" test-edit-file)
                           (cons "edits" (list (list (cons "oldText" "ABCD")
                                                     (cons "newText" "1234"))
                                               (list (cons "oldText" "CDEF")
                                                     (cons "newText" "5678")))))))

  ;; 6. 无变化（产生相同内容）报错
  (check-catch 'error
    (pi-execute-edit (list (cons "path" test-edit-file)
                           (cons "edits" (list (list (cons "oldText" "ABCDEFGH")
                                                     (cons "newText" "ABCDEFGH")))))))

  ;; 7. 空 edits 报错
  (check-catch 'error
    (pi-execute-edit (list (cons "path" test-edit-file)
                           (cons "edits" '()))))

  ;; 8. 空 oldText 报错
  (check-catch 'error
    (pi-execute-edit (list (cons "path" test-edit-file)
                           (cons "edits" (list (list (cons "oldText" "")
                                                     (cons "newText" "foo")))))))

  ;; 9. 超时路径测试
  (check-catch 'error
    (pi-execute-edit (list (cons "path" test-edit-file)
                           (cons "edits" (list (list (cons "oldText" "ABC")
                                                     (cons "newText" "XYZ")))))
                     :timed-out? #t))

  ;; 清理
  (delete-file test-edit-file))

(check-report)
