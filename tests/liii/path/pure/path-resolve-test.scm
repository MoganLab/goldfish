(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

;; path-resolve
;; 解析为规范化的绝对路径(对齐 pathlib.Path.resolve 的简化版)。
;;
;; 语法
;; ----
;; (path-resolve path-value) -> path-value
;;
;; 返回值
;; ------
;; path-value
;; 绝对化 + 折叠 . / .. 段。注:因无 realpath 原语,不解析符号链接
;; (与 pathlib strict 语义有差异)。

(when (not (os-windows?))
  (check (path->string (path-resolve (path "/a/b/.."))) => "/a")
  (check (path->string (path-resolve (path "/a/./b"))) => "/a/b")
  (check (path->string (path-resolve (path "/a/../.."))) => "/")
  (check-true (path-absolute? (path-resolve (path "foo"))))
) ;when

(check-report)
