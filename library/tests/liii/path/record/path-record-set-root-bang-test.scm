;; path-record-set-root!
;; 原地替换路径记录的根分隔符字段。
;;
;; 语法
;; ----
;; (path-record-set-root! p root)
;;
;; 参数
;; ----
;; p : path?
;; 路径记录对象。
;; root : char 或 #f
;; 新根分隔符，#f 表示无根。
;;
;; 返回值
;; ----
;; unspecified
;; 原地修改，返回值未定义。

(import (liii check) (liii path) (liii path record))

(check-set-mode! 'report-failed)

(let ((p (path "/a/b")))
  (path-record-set-root! p #f)
  (check (path-record-root p) => #f)
  (path-record-set-root! p #\/)
  (check (path-record-root p) => #\/)
) ;let

(check-report)
