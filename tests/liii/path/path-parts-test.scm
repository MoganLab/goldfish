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
;; path-of-drive 构造 windows record,parts 含 anchor 首元素(对齐 PureWindowsPath('C:\\').parts)
(check (path-parts (path-of-drive #\c)) => #("C:\\"))

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
  ;; UNC: parts 含 share anchor 首元素(对齐 PureWindowsPath('\\srv\sh\a\b').parts)
  (check (path-parts (path "\\\\srv\\sh\\a\\b")) => #("\\\\srv\\sh\\" "a" "b"))
  ;; drive-absolute: 首元素 "C:\"
  (check (path-parts (path "C:\\tmp\\demo.txt")) => #("C:\\" "tmp" "demo.txt"))
  ;; drive-relative: 首元素 "C:"(对齐 PureWindowsPath('C:foo').parts)
  (check (path-parts (path "C:foo")) => #("C:" "foo"))
  ;; current-drive root: 首元素 "\"
  (check (path-parts (path "\\foo")) => #("\\" "foo"))
) ;when

(check-report)
