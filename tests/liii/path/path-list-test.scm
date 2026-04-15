(import (liii check)
  (liii path)
  (liii string)
  (liii os)
  (liii vector)
) ;import

(check-set-mode! 'report-failed)

;; path-list
;; 列出目录中的文件名。
;;
;; 语法
;; ----
;; (path-list path) → vector
;;
;; 参数
;; ----
;; path : string | path-value
;; 要列出的目录路径。
;;
;; 返回值
;; -----
;; vector
;; 返回包含文件名字符串的向量。
;;
;; 描述
;; ----
;; path-list 返回字符串向量，每个元素是目录中的文件名。

;; 目录列举测试
(let* ((list-dir (path-join (path-temp-dir)
                   "path-list-dir"
                 ) ;path-join
       ) ;list-dir
       (list-file-a (path-join list-dir "child-a.txt")
       ) ;list-file-a
       (list-file-b (path-join list-dir "child-b.txt")
       ) ;list-file-b
      ) ;
  ;; 清理
  (when (path-exists? list-file-a)
    (delete-file (path->string list-file-a))
  ) ;when
  (when (path-exists? list-file-b)
    (delete-file (path->string list-file-b))
  ) ;when
  (when (path-exists? list-dir)
    (rmdir (path->string list-dir))
  ) ;when
  ;; 创建测试目录和文件
  (mkdir (path->string list-dir))
  (path-write-text list-file-a "a")
  (path-write-text list-file-b "b")

  (check-true (vector? (path-list list-dir))
  ) ;check-true
  (check-true (vector-contains? (path-list list-dir)
                "child-a.txt"
              ) ;vector-contains?
  ) ;check-true
  (check-true (vector-contains? (path-list list-dir)
                "child-b.txt"
              ) ;vector-contains?
  ) ;check-true

  ;; 清理
  (delete-file (path->string list-file-a))
  (delete-file (path->string list-file-b))
  (rmdir (path->string list-dir))
) ;let*

(check-report)
