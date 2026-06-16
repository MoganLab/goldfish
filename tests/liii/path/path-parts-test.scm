(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

;; path-parts
;; 获取路径的各个部分。
;;
;; 语法
;; ----
;; (path-parts path-value)
;;
;; 参数
;; ----
;; path-value : path-value
;; 要查询的路径值。
;;
;; 返回值
;; ----
;; vector
;; 返回包含路径各部分的字符串向量。

(check (path-parts (path)) => #("."))
(check (path-parts (path-root)) => #("/"))
(check (path-parts (path-of-drive #\c)) => #())

(when (not (os-windows?))
  (check (path-parts (path-from-parts #("/" "tmp" "demo.txt")))
    =>
    #("/" "tmp" "demo.txt")
  ) ;check
  ;; 绝对路径解析后,parts 对外加 "/" stub 表示绝对性
  (check (path-parts (path "/tmp/demo.txt")) => #("/" "tmp" "demo.txt"))
  ;; 相对路径不加 stub
  (check (path-parts (path "tmp/demo.txt")) => #("tmp" "demo.txt"))
) ;when

(when (os-windows?)
  ;; UNC: drive 字段表达绝对性,parts 不加 stub
  (check (path-parts (path "\\\\srv\\sh\\a\\b")) => #("a" "b"))
  ;; drive-absolute 同理
  (check (path-parts (path "C:\\tmp\\demo.txt")) => #("tmp" "demo.txt"))
) ;when

(check-report)
