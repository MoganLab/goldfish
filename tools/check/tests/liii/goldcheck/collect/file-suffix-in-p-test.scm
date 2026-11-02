;; file-suffix-in?
;; 判断路径文件名是否以指定后缀列表中的任一后缀（不含点）结尾。
;;
;; 语法
;; ----
;; (file-suffix-in? s suffixes)
;;
;; 参数
;; ----
;; s : string         文件路径
;; suffixes : list    后缀名字符串列表（如 '("scm" "json")）
;;
;; 返回值
;; ------
;; boolean
;; 匹配任一后缀返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii goldcheck collect))

(check-set-mode! 'report-failed)

;; 1. 匹配存在的后缀
(check (file-suffix-in? "foo.scm" '("scm" "json")) => #t)
(check (file-suffix-in? "path/to/bar.json" '("scm" "json")) => #t)

;; 2. 后缀不匹配
(check (file-suffix-in? "baz.txt" '("scm" "json")) => #f)
(check (file-suffix-in? "no-ext" '("scm")) => #f)
(check (file-suffix-in? "scm" '("scm")) => #f)

(check-report)
