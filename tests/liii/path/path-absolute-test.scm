(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

;; path-absolute
;; 返回绝对路径(对齐 pathlib.Path.absolute)。
;;
;; 语法
;; ----
;; (path-absolute path-value) -> path-value
;;
;; 返回值
;; ------
;; path-value
;; 相对路径拼 cwd,绝对路径保持。不解析符号链接(区别于 path-resolve)。

(when (not (os-windows?))
  (check-true (path-absolute? (path-absolute (path "foo"))))
  (check (path->string (path-absolute (path "/a/b"))) => "/a/b")
) ;when

(check-report)
