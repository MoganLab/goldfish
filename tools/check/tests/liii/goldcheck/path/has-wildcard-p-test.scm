;; has-wildcard?
;; 判断模式字符串中是否包含通配符 *。
;;
;; 语法
;; ----
;; (has-wildcard? pattern)
;;
;; 参数
;; ----
;; pattern : string   待判定的模式字符串
;;
;; 返回值
;; ------
;; boolean
;; 包含通配符返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii goldcheck path))

(check-set-mode! 'report-failed)

;; 1. 包含通配符返回 #t
(check (has-wildcard? "goldfish/*") => #t)
(check (has-wildcard? "*.scm") => #t)
(check (has-wildcard? "foo/*/bar") => #t)

;; 2. 不包含通配符返回 #f
(check (has-wildcard? "goldfish/liii/base.scm") => #f)
(check (has-wildcard? "") => #f)

(check-report)
