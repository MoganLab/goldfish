;; drop-dot-parts
;; 过滤路径段向量中的 "." 段与空段。
;;
;; 语法
;; ----
;; (drop-dot-parts v)
;;
;; 参数
;; ----
;; v : vector?
;; 路径段向量。
;;
;; 返回值
;; ----
;; vector?
;; 去掉 "." 与空段后的新向量，保留 ".."。
;;
;; 描述
;; ----
;; pathlib 风格：构造时丢弃 "." 与连续/尾部分隔符产生的空段，
;; 保留 ".." 直到 resolve 才处理。

(import (liii check) (liii path parse))

(check-set-mode! 'report-failed)

(check (drop-dot-parts #("a" "." "" "b")) => #("a" "b"))
(check (drop-dot-parts #(".." "a")) => #(".." "a"))
(check (drop-dot-parts #("")) => #())
(check (drop-dot-parts #()) => #())

(check-report)
