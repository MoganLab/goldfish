(import (liii check)
  (liii path)
  (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-getsize
;; 获取文件或目录的大小（字节数）。
;;
;; 语法
;; ----
;; (path-getsize path) → integer
;;
;; 参数
;; ----
;; path : string | path-value
;; 要获取大小的文件或目录路径。
;;
;; 返回值
;; -----
;; integer
;; 返回文件或目录的字节大小。
;;
;; 错误处理
;; ----
;; file-not-found-error 当路径不存在时。

;; 系统路径大小测试
(check-true (> (path-getsize (path-root)) 0)
) ;check-true

(when (not (os-windows?))
  (check-true (> (path-getsize "/etc/hosts") 0)
  ) ;check-true
  (check-true (> (path-getsize "/tmp") 0))
) ;when

;; 临时文件大小测试
(let ((size-file (path-join (path-temp-dir)
                   "path-getsize-test.txt"
                 ) ;path-join
      ) ;size-file
     ) ;
  (when (path-exists? size-file)
    (delete-file (path->string size-file))
  ) ;when
  (path-write-text size-file "")
  (check (path-getsize size-file) => 0)
  (path-write-text size-file "test")
  (check (path-getsize size-file) => 4)
  (path-write-text size-file
    "hello world test content"
  ) ;path-write-text
  (check (path-getsize size-file) => 24)
  (path-write-text size-file
    "中文测试"
  ) ;path-write-text
  (check (path-getsize size-file) => 12)
  (delete-file (path->string size-file))
) ;let

;; 错误处理测试
(check-catch 'file-not-found-error
  (path-getsize (path "/this/file/does/not/exist")
  ) ;path-getsize
) ;check-catch

(check-report)
