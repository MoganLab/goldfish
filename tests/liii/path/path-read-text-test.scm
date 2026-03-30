(import (liii check)
        (liii path)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-read-text
;; 读取文本文件内容。
;;
;; 语法
;; ----
;; (path-read-text path) → string
;;
;; 参数
;; ----
;; path : string | path-value
;; 要读取的文件路径。
;;
;; 返回值
;; -----
;; string
;; 返回文件的文本内容。
;;
;; 错误处理
;; ----
;; file-not-found-error 当文件不存在时。

;; 基本文本读写测试
(let ((text-file (path-join (path-temp-dir) "path-read-text-basic.txt")))
  (when (path-exists? text-file)
    (delete-file (path->string text-file)))
  (path-write-text text-file "Hello, World!")
  (check (path-read-text text-file) => "Hello, World!")
  (check (path-read-text (path->string text-file)) => "Hello, World!")
  (delete-file (path->string text-file))
) ;let

;; 空文件测试
(let ((empty-file (path-join (path-temp-dir) "path-read-text-empty.txt")))
  (when (path-exists? empty-file)
    (delete-file (path->string empty-file)))
  (path-write-text empty-file "")
  (check (path-read-text empty-file) => "")
  (delete-file (path->string empty-file))
) ;let

;; 中文文本测试
(let ((chinese-file (path-join (path-temp-dir) "path-read-text-zh.txt")))
  (when (path-exists? chinese-file)
    (delete-file (path->string chinese-file)))
  (path-write-text chinese-file "你好，世界！\n这是一段中文测试文本。")
  (check (path-read-text chinese-file) => "你好，世界！\n这是一段中文测试文本。")
  (delete-file (path->string chinese-file))
) ;let

;; 大文件读取测试
(let ((big-file (path-join (path-temp-dir) "path-read-text-large.txt"))
      (large-content (make-string 10000 #\a)))
  (when (path-exists? big-file)
    (delete-file (path->string big-file)))
  (path-write-text big-file large-content)
  (let ((read-content (path-read-text big-file)))
    (check (string-length read-content) => 10000)
    (check (string=? read-content large-content) => #t)
  ) ;let
  (delete-file (path->string big-file))
) ;let

;; 错误处理测试
(check-catch 'file-not-found-error (path-read-text (path "/this/file/does/not/exist")))

(check-report)
