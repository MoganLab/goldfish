;; path-record-root
;; 读取路径记录的根分隔符字段。
;;
;; 语法
;; ----
;; (path-record-root p)
;;
;; 参数
;; ----
;; p : path?
;; 路径记录对象。
;;
;; 返回值
;; ----
;; char 或 #f
;; #\/ 或 #\\ 表示有根分隔符；#f 表示无根。

(import (liii check) (liii path) (liii path record))

(check-set-mode! 'report-failed)

(check (path-record-root (path "/a/b")) => #\/)
(check (path-record-root (path "a/b")) => #f)

(check-report)
