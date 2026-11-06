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

(import (liii check) (liii path) (liii path record) (liii os))

(check-set-mode! 'report-failed)

;; path 按平台解析根分隔符：posix 为 #\/，windows 为 #\\
(when (not (os-windows?))
  (check (path-record-root (path "/a/b")) => #\/)
) ;when

(when (os-windows?)
  (check (path-record-root (path "C:\\a\\b")) => #\\)
) ;when

(check (path-record-root (path "a/b")) => #f)

(check-report)
