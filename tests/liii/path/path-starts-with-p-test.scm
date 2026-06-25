(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

;; path-starts-with?
;; 判断给定路径是否以另一路径开头（即是否为其本身或后代路径）。
;;
;; 语法
;; ----
;; (path-starts-with? p base)
;;
;; 参数
;; ----
;; p : string? | path-value
;; 待检查的路径。
;; base : string? | path-value
;; 作为基准的父路径。
;;
;; 返回值
;; ------
;; boolean
;; 当 p 是 base 本身或 base 的后代路径时返回 #t，否则返回 #f。

;; 基本功能测试
(check (path-starts-with? "/home/da/git" "/home/da") => #t)
(check (path-starts-with? "/home/da/git/goldfish" "/home/da") => #t)
(check (path-starts-with? "/home/da" "/home/da") => #t)
(check (path-starts-with? "/home/db" "/home/da") => #f)
(check (path-starts-with? "/home" "/home/da") => #f)

;; 接受 path-value
(check (path-starts-with? (path "/home/da/git") (path "/home/da")) => #t)

;; 相对路径
(check (path-starts-with? "tools/fmt/liii" "tools/fmt") => #t)
(check (path-starts-with? "tools/fmt/liii/goldfmt-cache.scm" "tools/fmt") => #t)
(check (path-starts-with? "tools/doc" "tools/fmt") => #f)

;; 跨锚点返回 #f
(when (os-windows?)
  (check (path-starts-with? "D:/a/b" "C:/a") => #f)
) ;when

(check-report)
