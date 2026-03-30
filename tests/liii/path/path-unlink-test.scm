(import (liii check)
        (liii path)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-unlink
;; 删除文件。
;;
;; 语法
;; ----
;; (path-unlink path [missing-ok]) → boolean
;;
;; 参数
;; ----
;; path : string | path-value
;; 要删除的文件路径。
;; missing-ok : boolean (可选)
;; 当为 #t 时，文件不存在不报错。
;;
;; 返回值
;; -----
;; boolean
;; 返回 #t 表示删除成功。
;;
;; 错误处理
;; ----
;; file-not-found-error 当文件不存在且 missing-ok 为 #f 时。

;; 文件删除测试
(let* ((unlink-dir (path-join (path-temp-dir) "path-unlink-dir"))
       (unlink-file-a (path-join unlink-dir "child-a.txt"))
       (unlink-file-b (path-join unlink-dir "child-b.txt")))
  ;; 清理
  (when (path-exists? unlink-file-a)
    (delete-file (path->string unlink-file-a)))
  (when (path-exists? unlink-file-b)
    (delete-file (path->string unlink-file-b)))
  (when (path-exists? unlink-dir)
    (rmdir (path->string unlink-dir)))
  ;; 创建
  (mkdir (path->string unlink-dir))
  (path-write-text unlink-file-a "a")
  (path-write-text unlink-file-b "b")

  ;; 测试删除
  (check-true (path-unlink unlink-file-a))
  (check-false (path-exists? unlink-file-a))

  ;; missing-ok 测试
  (check-true (path-unlink unlink-file-a #t))
  (check-catch 'file-not-found-error (path-unlink unlink-file-a))

  ;; 清理
  (check-true (path-unlink unlink-file-b))
  (check-true (path-rmdir unlink-dir))
  (check-false (path-exists? unlink-dir))
) ;let*

(check-report)
