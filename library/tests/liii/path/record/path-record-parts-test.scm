;; path-record-parts
;; 读取路径记录的分量向量字段。
;;
;; 语法
;; ----
;; (path-record-parts p)
;;
;; 参数
;; ----
;; p : path?
;; 路径记录对象。
;;
;; 返回值
;; ----
;; vector?
;; 路径分量向量（不含 anchor）。

(import (liii check) (liii path) (liii path record))

(check-set-mode! 'report-failed)

(check (path-record-parts (path "/a/b")) => #("a" "b"))
(check (path-record-parts (path "a")) => #("a"))

(check-report)
