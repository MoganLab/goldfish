;; strip-scm
;; 若路径字符串以 '.scm' 结尾则截除该后缀，否则原样返回。
;;
;; 语法
;; ----
;; (strip-scm s)
;;
;; 参数
;; ----
;; s : string  文件或路径字符串
;;
;; 返回值
;; ------
;; string
;; 去除 '.scm' 后的字符串。

(import (scheme base) (liii check) (liii goldcheck type-hint util))

(check-set-mode! 'report-failed)

;; 1. 去除 .scm 后缀
(check (strip-scm "foo.scm") => "foo")
(check (strip-scm "path/to/bar.scm") => "path/to/bar")

;; 2. 非 .scm 后缀原样返回
(check (strip-scm "baz.txt") => "baz.txt")
(check (strip-scm "hello") => "hello")

(check-report)
