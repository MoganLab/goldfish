(import (liii check)
  (liii os)
  (liii path stat)
  (scheme file)
) ;import

(check-set-mode! 'report-failed)

;; path-stat
;; 获取文件或目录的元数据记录对象（包含 size、mtime、atime、ctime、mode）。
;;
;; 语法
;; ----
;; (path-stat path) → <path-stat>
;;
;; 参数
;; ----
;; path : string
;; 要获取元数据的文件或目录路径字符串。
;;
;; 返回值
;; -----
;; <path-stat>
;; 返回 <path-stat> 记录对象。
;;
;; 错误处理
;; --------
;; file-not-found-error 当目标路径不存在时。
;; type-error 当参数不是字符串时。

;; 测试常规文件
(let ((test-file (string-append (os-temp-dir) "/path-stat-test-1050.txt")))
  (when (file-exists? test-file)
    (delete-file test-file)
  ) ;when
  (with-output-to-file test-file (lambda () (display "hello world")))
  (let ((st (path-stat test-file)))
    (check-true (path-stat? st))
    (check (path-stat-size st) => 11)
  ) ;let
  (delete-file test-file)
) ;let

;; 测试目录
(let* ((tmp-dir (os-temp-dir)) (st-dir (path-stat tmp-dir)))
  (check-true (path-stat? st-dir))
  (check-true (path-stat-dir? st-dir))
) ;let*

;; 错误处理
(check-catch 'file-not-found-error
  (path-stat "non-existent-path-for-1050-test-xyz")
) ;check-catch

(check-catch 'type-error (path-stat 123))
(check-catch 'type-error (path-stat '("not" "a" "path")))
(check-catch 'type-error (path-stat #t))
(check-catch 'type-error (path-stat (vector 1 2 3)))

(check-report)
