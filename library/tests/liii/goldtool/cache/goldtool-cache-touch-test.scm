;; goldtool-cache-touch
;; 创建缓存条目文件（空文件），必要时创建其父目录。
;; 并发安全：多个 worker 同时创建同一目录时，已被并发创建视为成功。
;;
;; 语法
;; ----
;; (goldtool-cache-touch file-path)
;;
;; 参数
;; ----
;; file-path : string?
;; 缓存条目文件的路径，父目录不存在时会被自动创建。
;;
;; 返回值
;; ------
;; unspecified
;; 条目文件被创建后返回。

(import (scheme base)
  (liii check)
  (liii path)
  (liii tempfile)
  (liii goldtool cache)
) ;import

(check-set-mode! 'report-failed)

(with-temp-dir
  (lambda (dir)
    (let* ((entry (path->string (path-join (path dir) "nested" "entry.cache"))))
      ;; 父目录不存在时自动创建
      (check (path-dir? (path (path->string (path-parent (path entry))))) => #f)
      (goldtool-cache-touch entry)
      (check (path-file? (path entry)) => #t)
      (check (goldtool-cache-hit? entry) => #t)
      ;; 重复 touch 不报错
      (goldtool-cache-touch entry)
      (check (path-file? (path entry)) => #t)
    ) ;let
  ) ;lambda
  "goldtool-cache-touch"
) ;with-temp-dir

(check-report)
