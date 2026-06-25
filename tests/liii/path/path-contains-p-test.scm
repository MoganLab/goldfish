(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

;; path-contains?
;; 判断给定路径是否为另一路径本身或其子路径。
;;
;; 语法
;; ----
;; (path-contains? base path)
;;
;; 参数
;; ----
;; base : string? | path-value
;; 作为基准的父路径。
;; path : string? | path-value
;; 待检查的路径。
;;
;; 返回值
;; ------
;; boolean
;; 当 path 是 base 本身或 base 的后代路径时返回 #t，否则返回 #f。

;; 基本功能测试
(check (path-contains? "/home/da" "/home/da/git") => #t)
(check (path-contains? "/home/da" "/home/da/git/goldfish") => #t)
(check (path-contains? "/home/da" "/home/da") => #t)
(check (path-contains? "/home/da" "/home/db") => #f)
(check (path-contains? "/home/da" "/home") => #f)

;; 接受 path-value
(check (path-contains? (path "/home/da") (path "/home/da/git")) => #t)

;; 相对路径
(check (path-contains? "tools/fmt" "tools/fmt/liii") => #t)
(check (path-contains? "tools/fmt" "tools/fmt/liii/goldfmt-cache.scm") => #t)
(check (path-contains? "tools/fmt" "tools/doc") => #f)

;; 跨锚点返回 #f
(when (os-windows?)
  (check (path-contains? "C:/a" "D:/a/b") => #f)
) ;when

(check-report)
