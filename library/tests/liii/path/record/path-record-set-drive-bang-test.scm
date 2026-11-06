;; path-record-set-drive!
;; 原地替换路径记录的盘符字段。
;;
;; 语法
;; ----
;; (path-record-set-drive! p drive)
;;
;; 参数
;; ----
;; p : path?
;; 路径记录对象。
;; drive : string?
;; 新盘符（或空串）。
;;
;; 返回值
;; ----
;; unspecified
;; 原地修改，返回值未定义。

(import (liii check) (liii path) (liii path record))

(check-set-mode! 'report-failed)

(let ((p (path "/a/b")))
  (path-record-set-drive! p "D")
  (check (path-record-drive p) => "D")
) ;let

(check-report)
