;; glob-match?
;; 判断字符串是否匹配指定的通配符模式（支持 * 匹配任意字符序列）。
;;
;; 语法
;; ----
;; (glob-match? pattern str)
;;
;; 参数
;; ----
;; pattern : string   通配符模式字符串
;; str : string       待匹配的目标字符串
;;
;; 返回值
;; ------
;; boolean
;; 匹配成功返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii goldcheck path))

(check-set-mode! 'report-failed)

;; 1. 精确匹配
(check (glob-match? "foo.scm" "foo.scm") => #t)
(check (glob-match? "foo.scm" "bar.scm") => #f)

;; 2. 尾部通配符匹配
(check (glob-match? "goldfish/*" "goldfish/liii/base.scm") => #t)
(check (glob-match? "*.scm" "test.scm") => #t)

;; 3. 中间通配符匹配
(check (glob-match? "goldfish/*/base.scm" "goldfish/liii/base.scm") => #t)

;; 4. 不匹配的情形
(check (glob-match? "*.scm" "test.json") => #f)
(check (glob-match? "foo*" "bar") => #f)

(check-report)
