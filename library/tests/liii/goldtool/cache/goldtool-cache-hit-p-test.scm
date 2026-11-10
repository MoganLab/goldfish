;; goldtool-cache-hit?
;; 判断缓存条目文件是否存在；条目存在即表示对应内容已通过检查。
;;
;; 语法
;; ----
;; (goldtool-cache-hit? file-path)
;;
;; 参数
;; ----
;; file-path : string?
;; 缓存条目文件的路径。
;;
;; 返回值
;; ------
;; boolean
;; 条目文件存在返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii path)
  (liii tempfile)
  (liii goldtool cache)
) ;import

(check-set-mode! 'report-failed)

(with-temp-dir
  (lambda (dir)
    (let ((entry (path->string (path-join (path dir) "entry.cache")))) ;
      ;; 条目不存在时未命中
      (check (goldtool-cache-hit? entry) => #f)
      ;; 条目存在时命中
      (path-write-text (path entry) "")
      (check (goldtool-cache-hit? entry) => #t)
    ) ;let
  ) ;lambda
  "goldtool-cache-hit"
) ;with-temp-dir

(check-report)
