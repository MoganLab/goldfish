;; normalize-sep
;; 将路径分隔符统一替换为正斜杠，并移除开头的 ./ 前缀。
;;
;; 语法
;; ----
;; (normalize-sep s)
;;
;; 参数
;; ----
;; s : string   待归一化的路径字符串
;;
;; 返回值
;; ------
;; string
;; 归一化后的路径字符串。

(import (scheme base) (liii check) (liii goldcheck path))

(check-set-mode! 'report-failed)

;; 1. 反斜杠替换为正斜杠
(check (normalize-sep "foo\\bar\\baz.scm") => "foo/bar/baz.scm")

;; 2. 移除开头的 ./ 前缀
(check (normalize-sep "./foo/bar.scm") => "foo/bar.scm")
(check (normalize-sep ".\\foo\\bar.scm") => "foo/bar.scm")

;; 3. 无需转换的路径保持原样
(check (normalize-sep "foo/bar.scm") => "foo/bar.scm")

(check-report)
