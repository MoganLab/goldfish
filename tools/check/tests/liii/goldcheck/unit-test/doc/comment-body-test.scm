;; comment-body
;; 提取注释行中分号后的有效文本内容。
;;
;; 语法
;; ----
;; (comment-body line)
;;
;; 参数
;; ----
;; line : string  代码文本行
;;
;; 返回值
;; ------
;; string 或 #f
;; 若为分号开头的注释行，返回去除分号及前导空格后的正文；若非注释行则返回 #f。

(import (scheme base) (liii check) (liii goldcheck unit-test doc))

(check-set-mode! 'report-failed)

;; 1. 注释行正文提取
(check (comment-body ";; 函数说明") => "函数说明")
(check (comment-body ";;   缩进说明") => "缩进说明")
(check (comment-body ";") => "")
(check (comment-body ";;; ----") => "----")

;; 2. 非注释行返回 #f
(check (comment-body "(define x 1)") => #f)
(check (comment-body "   ") => #f)

(check-report)
