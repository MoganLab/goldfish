;; path-record-set-parts!
;; 原地替换路径记录的分量向量字段。
;;
;; 语法
;; ----
;; (path-record-set-parts! p parts)
;;
;; 参数
;; ----
;; p : path?
;; 路径记录对象。
;; parts : vector?
;; 新的分量向量。
;;
;; 返回值
;; ----
;; unspecified
;; 原地修改，返回值未定义。

(import (liii check) (liii path) (liii path record) (liii os))

(check-set-mode! 'report-failed)

(let ((p (path "/a/b")))
  (path-record-set-parts! p #("x" "y"))
  (check (path-record-parts p) => #("x" "y"))
  ;; path->string 按记录的 type 输出分隔符
  (if (os-windows?)
    (check (path->string p) => "\\x\\y")
    (check (path->string p) => "/x/y")
  ) ;if
) ;let

(check-report)
