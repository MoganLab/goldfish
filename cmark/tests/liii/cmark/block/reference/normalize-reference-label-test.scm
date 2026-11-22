;; normalize-reference-label
;; 规范化链接引用标签（折叠内部空白并执行 Unicode 大小写折叠）。
;;
;; 语法
;; ----
;; (normalize-reference-label label)
;;
;; 参数
;; ----
;; label : string
;; 原始引用标签字符串。
;;
;; 返回值
;; ----
;; string
;; 规范化后的标签字符串。

(import (scheme base))
(import (liii check))
(import (liii cmark block reference))

(check-set-mode! 'report-failed)

(check (normalize-reference-label "  Foo   BAR ") => "foo bar")

(check-report)
