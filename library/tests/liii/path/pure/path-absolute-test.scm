;; path-absolute
;; 返回绝对路径(对齐 pathlib.Path.absolute)。
;;
;; 语法
;; ----
;; (path-absolute path-value) -> path-value
;;
;; 参数
;; ----
;; path-value : path?
;; 待处理的输入路径。
;;
;; 返回值
;; ------
;; path-value
;; 相对路径拼 cwd,绝对路径保持。不解析符号链接(区别于 path-resolve)。

(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

(when (not (os-windows?))
  (check-true (path-absolute? (path-absolute (path "foo"))))
  (check (path->string (path-absolute (path "/a/b"))) => "/a/b")
) ;when

(check-report)
