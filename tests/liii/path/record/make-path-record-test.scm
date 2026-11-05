;; make-path-record
;; 直接构造路径记录对象。
;;
;; 语法
;; ----
;; (make-path-record parts type drive root)
;;
;; 参数
;; ----
;; parts : vector?
;; 路径段向量。
;; type : symbol
;; 路径类型，'posix 或 'windows。
;; drive : string?
;; 盘符或 UNC 主机名，posix 为空串。
;; root : char 或 #f
;; 根分隔符，#f 表示无根。
;;
;; 返回值
;; ----
;; path?
;; 新构造的路径记录对象。

(import (liii check) (liii path record))

(check-set-mode! 'report-failed)

(let ((p (make-path-record #("a" "b") 'posix "" #\/)))
  (check (path? p) => #t)
  (check (path-record-parts p) => #("a" "b"))
  (check (path-record-type p) => 'posix)
  (check (path-record-drive p) => "")
  (check (path-record-root p) => #\/)
) ;let

(check-report)
