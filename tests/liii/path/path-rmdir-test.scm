(import (liii check)
        (liii path)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-rmdir
;; 删除空目录。
;;
;; 语法
;; ----
;; (path-rmdir path) → boolean
;;
;; 参数
;; ----
;; path : string | path-value
;; 要删除的目录路径。
;;
;; 返回值
;; -----
;; boolean
;; 返回 #t 表示删除成功。

;; 目录删除测试
(let* ((rmdir-test-dir (path-join (path-temp-dir) "path-rmdir-test"))
       (file-in-dir (path-join rmdir-test-dir "file.txt")))
  ;; 清理残留
  (when (path-exists? file-in-dir)
    (delete-file (path->string file-in-dir)))
  (when (path-exists? rmdir-test-dir)
    (rmdir (path->string rmdir-test-dir)))
  ;; 创建目录和文件
  (mkdir (path->string rmdir-test-dir))
  (path-write-text file-in-dir "content")
  ;; 删除文件和目录
  (check-true (path-unlink file-in-dir))
  (check-false (path-exists? file-in-dir))
  (check-true (path-rmdir rmdir-test-dir))
  (check-false (path-exists? rmdir-test-dir))
) ;let*

(check-report)
