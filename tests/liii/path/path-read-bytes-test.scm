(import (liii check)
  (liii path)
  (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-read-bytes
;; 以 bytevector 形式从文件中读取二进制数据。
;;
;; 语法
;; ----
;; (path-read-bytes path) → bytevector
;;
;; 参数
;; ----
;; path : string | path-value
;; 文件路径（可以是绝对路径或相对路径）
;;
;; 返回值
;; ----
;; bytevector
;; 二进制文件数据
;;
;; 错误处理
;; ----
;; file-not-found-error 当文件不存在时。

;; 基本二进制文件测试
(let ((binary-file (path-join (path-temp-dir)
                     "path-read-bytes-basic.dat"
                   ) ;path-join
      ) ;binary-file
     ) ;
  (when (path-exists? binary-file)
    (delete-file (path->string binary-file))
  ) ;when
  (path-write-text binary-file
    "Hello, binary world!"
  ) ;path-write-text
  (let ((read-content (path-read-bytes binary-file)
        ) ;read-content
       ) ;
    (check-true (bytevector? read-content))
    (check (bytevector-length read-content)
      =>
      20
    ) ;check
    (check (utf8->string read-content)
      =>
      "Hello, binary world!"
    ) ;check
  ) ;let
  (delete-file (path->string binary-file))
) ;let

;; 空二进制文件测试
(let ((empty-file (path-join (path-temp-dir)
                    "path-read-bytes-empty.dat"
                  ) ;path-join
      ) ;empty-file
     ) ;
  (when (path-exists? empty-file)
    (delete-file (path->string empty-file))
  ) ;when
  (path-write-text empty-file "")
  (let ((empty-bytes (path-read-bytes empty-file)
        ) ;empty-bytes
       ) ;
    (check (bytevector-length empty-bytes)
      =>
      0
    ) ;check
  ) ;let
  (delete-file (path->string empty-file))
) ;let

;; 中文文件名二进制读取测试
(let ((chinese-binary (path-join (path-temp-dir)
                        "中文_测试数据.bin"
                      ) ;path-join
      ) ;chinese-binary
     ) ;
  (when (path-exists? chinese-binary)
    (delete-file (path->string chinese-binary)
    ) ;delete-file
  ) ;when
  (path-write-text chinese-binary
    "\x01;\x02;\x03;\x04;\x05;"
  ) ;path-write-text
  (let ((read-chinese (path-read-bytes chinese-binary)
        ) ;read-chinese
       ) ;
    (check-true (bytevector? read-chinese))
    (check-true (> (bytevector-length read-chinese) 0)
    ) ;check-true
  ) ;let
  (delete-file (path->string chinese-binary)
  ) ;delete-file
) ;let

;; 与 path-read-text 的对比测试
(let ((comparison-file (path-join (path-temp-dir)
                         "path-read-bytes-comparison.dat"
                       ) ;path-join
      ) ;comparison-file
     ) ;
  (when (path-exists? comparison-file)
    (delete-file (path->string comparison-file)
    ) ;delete-file
  ) ;when
  (path-write-text comparison-file
    "Hello, World!测试"
  ) ;path-write-text
  (let ((binary-data (path-read-bytes comparison-file)
        ) ;binary-data
       ) ;
    (check-true (bytevector? binary-data))
    (check (utf8->string binary-data)
      =>
      "Hello, World!测试"
    ) ;check
  ) ;let
  (check (path-read-text comparison-file)
    =>
    "Hello, World!测试"
  ) ;check
  (delete-file (path->string comparison-file)
  ) ;delete-file
) ;let

;; 错误处理测试
(check-catch 'file-not-found-error
  (path-read-bytes (path "/this/file/does/not/exist")
  ) ;path-read-bytes
) ;check-catch

(check-report)
