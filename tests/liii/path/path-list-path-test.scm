(import (liii check)
        (liii path)
        (liii string)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; 辅助函数
(define (string-list-contains? target xs)
  (cond ((null? xs) #f)
        ((string=? target (car xs)) #t)
        (else (string-list-contains? target (cdr xs)))
  ) ;cond
) ;define

;; path-list-path
;; 列出目录中的路径值。
;;
;; 语法
;; ----
;; (path-list-path path) → vector
;;
;; 参数
;; ----
;; path : string | path-value
;; 要列出的目录路径。
;;
;; 返回值
;; -----
;; vector
;; 返回包含路径值的向量。
;;
;; 描述
;; ----
;; path-list-path 返回路径值向量，与 path-list 不同，返回的是路径值而非字符串。

;; 辅助函数
(define (path-vector->string-list xs)
  (vector->list (vector-map path->string xs))
) ;define

;; 目录列举测试
(let* ((list-dir (path-join (path-temp-dir) "path-list-path-dir"))
       (list-file-a (path-join list-dir "child-a.txt"))
       (list-file-b (path-join list-dir "child-b.txt")))
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

  (let ((listed-paths (path-list-path list-dir)))
    (check-true (vector? listed-paths))
    (check-true
      (string-list-contains? (path->string list-file-a)
                             (path-vector->string-list listed-paths)
      ) ;string-list-contains?
    ) ;check-true
    (check-true
      (string-list-contains? (path->string list-file-b)
                             (path-vector->string-list listed-paths)
      ) ;string-list-contains?
    ) ;check-true
  ) ;let

  ;; 清理
  (delete-file (path->string list-file-a))
  (delete-file (path->string list-file-b))
  (rmdir (path->string list-dir))
) ;let*

(check-report)
