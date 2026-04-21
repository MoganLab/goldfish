(import (liii check)
        (liii goldfmt-rule))

(check-set-mode! 'report-failed)

;; path-has-common-ancestor?
;; 判断两个路径是否有共同祖先（排除根目录 '/'）。
;;
;; 语法
;; ----
;; (path-has-common-ancestor? path1 path2)
;;
;; 参数
;; ----
;; path1 : string?
;; path2 : string?
;;
;; 返回值
;; ------
;; boolean?
;; 返回 #t 表示两个路径有除根目录外的共同祖先。

(check (path-has-common-ancestor? "/a/b/c" "/a/b/d") => #t)
(check (path-has-common-ancestor? "/a/b/c" "/a/x/y") => #t)
(check (path-has-common-ancestor? "/a/b/c" "/x/y/z") => #f)
(check (path-has-common-ancestor? "/usr/local/goldfish/bin/gf" "/usr/local/goldfish/goldfish") => #t)
(check (path-has-common-ancestor? "/usr/local/goldfish/bin/gf" "/home/da/.local/goldfish/lib") => #f)
(check (path-has-common-ancestor? "a/b/c" "a/x/y") => #t)
(check (path-has-common-ancestor? "a/b/c" "x/y/z") => #f)
(check (path-has-common-ancestor? "/" "/a/b") => #f)

(check-report)
