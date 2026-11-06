;; path-record-set-type!
;; 原地替换路径记录的类型字段。
;;
;; 语法
;; ----
;; (path-record-set-type! p type)
;;
;; 参数
;; ----
;; p : path?
;; 路径记录对象。
;; type : symbol
;; 新类型，'posix 或 'windows。
;;
;; 返回值
;; ----
;; unspecified
;; 原地修改，返回值未定义。

(import (liii check) (liii path) (liii path record))

(check-set-mode! 'report-failed)

(let ((p (path "/a/b")))
  (path-record-set-type! p 'windows)
  (check (path-record-type p) => 'windows)
) ;let

(check-report)
