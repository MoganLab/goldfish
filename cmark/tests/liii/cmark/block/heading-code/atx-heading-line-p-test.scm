;; atx-heading-line?
;; 判断行是否为合法 ATX 标题行。
;;
;; 语法
;; ----
;; (atx-heading-line? line)
;;
;; 参数
;; ----
;; line : string
;; 输入行。
;;
;; 返回值
;; ----
;; boolean
;; 是 ATX 标题返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block heading-code))

(check-set-mode! 'report-failed)

(check-true (atx-heading-line? "# Title"))
(check-false (atx-heading-line? "#NoSpace"))

(check-report)
