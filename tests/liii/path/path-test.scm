(import (liii check)
        (liii path)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path
;; 构造路径值。
;;
;; 语法
;; ----
;; (path [value])
;;
;; 参数
;; ----
;; value : string? | path-value? | 无
;; 可选的路径值或字符串，默认为 "."。
;;
;; 返回值
;; ----
;; path-value
;; 返回一个新的路径值。
;;
;; 注意
;; ----
;; 如果传入 path-value，会返回其副本。
;;
;; 示例
;; ----
;; (path->string (path)) => "."
;; (path->string (path "")) => "."
;; (path->string (path "tmp/demo.txt")) => "tmp/demo.txt"

(check (path->string (path)) => ".")
(check (path->string (path "")) => ".")

(when (not (os-windows?))
  (check (path->string (path "tmp/demo.txt")) => "tmp/demo.txt")
  (check (path->string (path (path "tmp/demo.txt"))) => "tmp/demo.txt")
) ;when

(when (os-windows?)
  (check (path->string (path "tmp/demo.txt")) => "tmp\\demo.txt")
  (check (path->string (path (path "tmp/demo.txt"))) => "tmp\\demo.txt")
) ;when

(check-report)
