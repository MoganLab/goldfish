(import (liii check)
  (liii os)
  (liii path stat)
  (scheme file)
) ;import

(check-set-mode! 'report-failed)

;; path-stat-size
;; 获取 <path-stat> 记录中的文件大小（字节数）。
;;
;; 语法
;; ----
;; (path-stat-size st)
;;
;; 参数
;; ----
;; st : <path-stat>
;; 元数据记录对象。
;;
;; 返回值
;; -----
;; integer
;; 文件大小字节数。

(let ((st (make-path-stat 1024 1.0 2.0 3.0 0)))
  (check (path-stat-size st) => 1024)
) ;let

(let ((test-file (string-append (os-temp-dir) "/path-stat-size-test.txt")))
  (when (file-exists? test-file)
    (delete-file test-file)
  ) ;when
  (with-output-to-file test-file (lambda () (display "1234567")))
  (let ((st (path-stat test-file)))
    (check (path-stat-size st) => 7)
  ) ;let
  (delete-file test-file)
) ;let

(check-catch 'type-error (path-stat-size 123))
(check-catch 'type-error (path-stat-size "not-a-record"))

(check-report)
