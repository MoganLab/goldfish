(import (liii check)
  (liii os)
  (liii path stat)
  (liii time)
  (scheme file)
) ;import

(check-set-mode! 'report-failed)

;; path-stat-mtime
;; 获取 <path-stat> 记录中的最后修改时间（Unix 时间戳，单位秒）。
;;
;; 语法
;; ----
;; (path-stat-mtime st)
;;
;; 参数
;; ----
;; st : <path-stat>
;; 元数据记录对象。
;;
;; 返回值
;; -----
;; real
;; 修改时间戳（秒）。

(let ((st (make-path-stat 0 12345.678 0 0 0)))
  (check (path-stat-mtime st) => 12345.678)
) ;let

(let ((test-file (string-append (os-temp-dir) "/path-stat-mtime-test.txt")))
  (when (file-exists? test-file)
    (delete-file test-file)
  ) ;when
  (with-output-to-file test-file (lambda () (display "mtime test")))
  (let* ((st (path-stat test-file)) (old-mtime (path-stat-mtime st)))
    (check-true (> old-mtime 0))
    (sleep 0.05)
    (with-output-to-file test-file (lambda () (display "modified content")))
    (let* ((st-new (path-stat test-file)) (new-mtime (path-stat-mtime st-new)))
      (check-true (>= new-mtime old-mtime))
    ) ;let*
  ) ;let*
  (delete-file test-file)
) ;let

(check-catch 'type-error (path-stat-mtime 123))
(check-catch 'type-error (path-stat-mtime "invalid"))

(check-report)
