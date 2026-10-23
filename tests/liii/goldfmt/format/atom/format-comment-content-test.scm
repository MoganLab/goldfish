(import (liii check))
(import (liii goldfmt format atom))

(check-set-mode! 'report-failed)

;; format-comment-content
;; 将注释内容格式化为 ;; 注释行：已有前导空格或分号的内容直接拼接，
;; 其余补一个空格。
;;
;; 语法
;; ----
;; (format-comment-content content)
;;
;; 参数
;; ----
;; content : string?
;;
;; 返回值
;; ------
;; string?

;; 测试：普通内容补空格
(check (format-comment-content "note") => ";; note")

;; 测试：前导空格内容不重复补
(check (format-comment-content " note") => ";; note")

;; 测试：以分号开头的内容不补空格
(check (format-comment-content ";nested") => ";;;nested")

;; 测试：空内容
(check (format-comment-content "") => ";;")

(check-report)
