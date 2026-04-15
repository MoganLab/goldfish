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
(let ((text-file (path-join (path-temp-dir)
                   "path-read-text-basic.txt"
                 ) ;path-join
      ) ;text-file
     ) ;
  (when (path-exists? text-file)
    (delete-file (path->string text-file))
  ) ;when
  (path-write-text text-file
    "Hello, World!"
  ) ;path-write-text
  (check (path-read-text text-file)
    =>
    "Hello, World!"
  ) ;check
  (check (path-read-text (path->string text-file)
         ) ;path-read-text
    =>
    "Hello, World!"
  ) ;check
  (delete-file (path->string text-file))
) ;let

;; 空文件测试
(let ((empty-file (path-join (path-temp-dir)
                    "path-read-text-empty.txt"
                  ) ;path-join
      ) ;empty-file
     ) ;
  (when (path-exists? empty-file)
    (delete-file (path->string empty-file))
  ) ;when
  (path-write-text empty-file "")
  (check (path-read-text empty-file)
    =>
    ""
  ) ;check
  (delete-file (path->string empty-file))
) ;let

;; 中文文本测试
(let ((chinese-file (path-join (path-temp-dir)
                      "path-read-text-zh.txt"
                    ) ;path-join
      ) ;chinese-file
     ) ;
  (when (path-exists? chinese-file)
    (delete-file (path->string chinese-file)
    ) ;delete-file
  ) ;when
  (path-write-text chinese-file
    "你好，世界！\n这是一段中文测试文本。"
  ) ;path-write-text
  (check (path-read-text chinese-file)
    =>
    "你好，世界！\n这是一段中文测试文本。"
  ) ;check
  (delete-file (path->string chinese-file)
  ) ;delete-file
) ;let

;; 大文件读取测试
(let ((big-file (path-join (path-temp-dir)
                  "path-read-text-large.txt"
                ) ;path-join
      ) ;big-file
      (large-content (make-string 10000 #\a))
     ) ;
  (when (path-exists? big-file)
    (delete-file (path->string big-file))
  ) ;when
  (path-write-text big-file large-content)
  (let ((read-content (path-read-text big-file))
       ) ;
    (check (string-length read-content)
      =>
      10000
    ) ;check
    (check (string=? read-content large-content)
      =>
      #t
    ) ;check
  ) ;let
  (delete-file (path->string big-file))
) ;let

;; 错误处理测试
(check-catch 'file-not-found-error
  (path-read-text (path "/this/file/does/not/exist")
  ) ;path-read-text
) ;check-catch

;; 测试 \r\n 自动转换为 \n
(let ((crlf-file (path-join (path-temp-dir)
                   "path-read-text-crlf.txt"
                 ) ;path-join
      ) ;crlf-file
     ) ;
  (when (path-exists? crlf-file)
    (delete-file (path->string crlf-file))
  ) ;when
  ;; 使用 C++ 的 g_path-write-text 直接写入包含 \r\n 的内容
  ;; 注意：这里我们使用 g_path-write-text C++ 函数确保 \r 被保留
  (g_path-write-text (path->string crlf-file)
    "Hello\r\nWorld\r\n"
  ) ;g_path-write-text
  ;; 读取时应自动将 \r\n 转换为 \n
  (check (path-read-text crlf-file)
    =>
    "Hello\nWorld\n"
  ) ;check
  (delete-file (path->string crlf-file))
) ;let

;; 测试独立的 \r 也转换为 \n (旧 Mac 格式)
(let ((cr-file (path-join (path-temp-dir)
                 "path-read-text-cr.txt"
               ) ;path-join
      ) ;cr-file
     ) ;
  (when (path-exists? cr-file)
    (delete-file (path->string cr-file))
  ) ;when
  ;; 写入只包含 \r 的内容
  (g_path-write-text (path->string cr-file)
    "Hello\rWorld\r"
  ) ;g_path-write-text
  ;; 读取时应自动将 \r 转换为 \n
  (check (path-read-text cr-file)
    =>
    "Hello\nWorld\n"
  ) ;check
  (delete-file (path->string cr-file))
) ;let

(check-report)
