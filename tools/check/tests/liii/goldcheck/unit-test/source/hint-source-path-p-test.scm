;; hint-source-path?
;; 判断路径是否位于 hint 目录（类型提示库目录）。
;;
;; 语法
;; ----
;; (hint-source-path? norm)
;;
;; 参数
;; ----
;; norm : string
;; 已归一化分隔符的路径字符串。
;;
;; 返回值
;; ------
;; boolean
;; 路径含 hint 目录段时返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; hint 目录存放的是类型提示库，不参与库源码与单元测试目录的互推导，
;; 因此单元测试规范检查需要将其排除。

(import (scheme base)
  (liii check)
  (liii goldcheck unit-test source)
) ;import

(check-set-mode! 'report-failed)

;; 1. 板块 hint 目录
(check-true (hint-source-path? "r7rs/hint/scheme/base.scm"))
(check-true (hint-source-path? "goldfish/hint/liii/path.scm"))
(check-true (hint-source-path? "tools/check/hint/liii/clean.scm"))

;; 2. 非 hint 路径
(check (hint-source-path? "r7rs/scheme/base.scm") => #f)
(check (hint-source-path? "r7rs/tests/scheme/base/assoc-test.scm") => #f)
(check (hint-source-path? "tools/check/tests/liii/goldcheck/path.scm") => #f)

(check-report)
