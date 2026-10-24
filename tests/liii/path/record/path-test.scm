(import (liii check) (liii path) (liii os))

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

;; drop-dot-parts "." 过滤: 对齐 pathlib.PurePath('a/./b').parts 不含 "."
(when (not (os-windows?))
  (check (path->string (path "a/./b")) => "a/b")
  (check (path->string (path "./a")) => "a")
  ;; ".." 必须保留,只有在 resolve() 才处理
  (check (path->string (path "a/../b")) => "a/../b")
) ;when

(when (os-windows?)
  (check (path->string (path "a\\.\\b")) => "a\\b")
  (check (path->string (path "C:\\")) => "C:\\")
  ;; UNC round-trip(对齐 pathlib): 光秃 server 不带尾斜杠;share anchor 带尾斜杠
  (check (path->string (path "\\\\srv")) => "\\\\srv")
  (check (path->string (path "\\\\srv\\sh")) => "\\\\srv\\sh\\")
  (check (path->string (path "\\\\srv\\sh\\a")) => "\\\\srv\\sh\\a")
) ;when

;; path-from-parts 边界: 空 vector、空 stub head
(check (path->string (path-from-parts #())) => ".")
(when (not (os-windows?))
  (check (path->string (path-from-parts #("" "tmp" "demo.txt")))
    =>
    "/tmp/demo.txt"
  ) ;check
) ;when

(check-report)
