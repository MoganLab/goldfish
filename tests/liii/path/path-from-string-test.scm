(import (liii check) (liii path))

(check-set-mode! 'report-failed)

;; path-from-string
;; 从字符串构造路径值，与 path 函数等价。
;;
;; 语法
;; ----
;; (path-from-string string-path)
;;
;; 参数
;; ----
;; string-path : string?
;; 字符串形式的路径。
;;
;; 返回值
;; ----
;; path-value
;; 返回对应的路径值。
;;
;; 示例
;; ----
;; (path->string (path-from-string "archive.tar.gz")) => "archive.tar.gz"

(check (path->string (path-from-string "archive.tar.gz")
       ) ;path->string
  =>
  "archive.tar.gz"
) ;check

(check-report)
